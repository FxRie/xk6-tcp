package tcp

import (
	"net"
	"time"

	k6modules "go.k6.io/k6/js/modules"
	k6metrics "go.k6.io/k6/metrics"
	// "io"
	// "bufio"
)

func init() {
	k6modules.Register("k6/x/tcpx", New())
}

type (
	// RootModule is the global module instance that will create module
	// instances for each VU.
	RootModule struct{}

	// ModuleInstance represents an instance of the JS module.
	ModuleInstance struct {
		// comparator is the exported type
		tcp *TCP
	}
)

// Ensure the interfaces are implemented correctly.
var (
	_ k6modules.Instance = &ModuleInstance{}
	_ k6modules.Module   = &RootModule{}
)

// New returns a pointer to a new RootModule instance.
func New() *RootModule {
	return &RootModule{}
}

// NewModuleInstance implements the modules.Module interface returning a new instance for each VU.
func (*RootModule) NewModuleInstance(vu k6modules.VU) k6modules.Instance {
	return &ModuleInstance{
		tcp: &TCP{vu: vu},
	}
}

// Exports implements the modules.Instance interface and returns the exported types for the JS module.
func (mi *ModuleInstance) Exports() k6modules.Exports {
	return k6modules.Exports{
		Default: mi.tcp,
	}
}

type TCP struct {
	vu k6modules.VU
}

func (tcp *TCP) Connect(address string) (*net.TCPConn, error) {
	addr, err := net.ResolveTCPAddr("tcp", address)
	conn, err := net.DialTCP("tcp", nil, addr)
	if err != nil {
		return nil, err
	}

	return conn, nil
}

func (tcp *TCP) Write(conn *net.TCPConn, data []byte) error {
	byteCount := len(data)
	_, err := conn.Write(data)
	if err != nil {
		return err
	}

	state := tcp.vu.State()
	dataSendMetric := state.BuiltinMetrics.DataSent
	k6metrics.PushIfNotDone(tcp.vu.Context(), state.Samples, k6metrics.Samples{
		k6metrics.Sample{
			TimeSeries: k6metrics.TimeSeries{
				Metric: dataSendMetric,
				Tags:   state.Tags.GetCurrentValues().Tags,
			},
			Value: float64(byteCount),
			Time:  time.Now().UTC(),
		},
	})

	return nil
}

func (tcp *TCP) Read(conn *net.TCPConn, size int) ([]byte, error) {
	buf := make([]byte, size)
	_, err := conn.Read(buf)
	if err != nil {
		return nil, err
	}

	state := tcp.vu.State()
	dataReceiveMetric := state.BuiltinMetrics.DataReceived
	k6metrics.PushIfNotDone(tcp.vu.Context(), state.Samples, k6metrics.Samples{
		k6metrics.Sample{
			TimeSeries: k6metrics.TimeSeries{
				Metric: dataReceiveMetric,
				Tags:   state.Tags.GetCurrentValues().Tags,
			},
			Value: float64(size),
			Time:  time.Now().UTC(),
		},
	})

	return buf, nil
}

func (tcp *TCP) WriteBytesLn(conn *net.TCPConn, data []byte, delim []byte) error {
	return tcp.Write(conn, append(data, delim...))
}

func (tcp *TCP) WriteLn(conn *net.TCPConn, data string, delim string) error {
	return tcp.Write(conn, append([]byte(data), []byte(delim)...))
}

func (tcp *TCP) Close(conn *net.TCPConn) error {
	err := conn.Close()
	if err != nil {
		return err
	}
	return nil
}

func (tcp *TCP) CloseWrite(conn *net.TCPConn) error {
	err := conn.CloseWrite()
	if err != nil {
		return err
	}
	return nil
}

func (tcp *TCP) CloseRead(conn *net.TCPConn) error {
	err := conn.CloseRead()
	if err != nil {
		return err
	}
	return nil
}
