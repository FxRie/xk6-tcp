package tcp

import (
	"net"
	// "net/TCPConn"
	"go.k6.io/k6/js/modules"
	// "io"
	// "bufio"
)

func init() {
	modules.Register("k6/x/tcp", new(TCP))
}

type TCP struct{}

func (tcp *TCP) Connect(address string) (*net.TCPConn, error) {
	addr, err := net.ResolveTCPAddr("tcp", address)
	conn, err := net.DialTCP("tcp", nil, addr)
	if err != nil {
		return nil, err
	}

	return conn, nil
}

func (tcp *TCP) Write(conn *net.TCPConn, data []byte) error {
	_, err := conn.Write(data)
	if err != nil {
		return err
	}

	return nil
}

func (tcp *TCP) Read(conn *net.TCPConn, size int) ([]byte, error) {
	buf := make([]byte, size)
	_, err := conn.Read(buf)
	if err != nil {
		return nil, err
	}
	return buf, nil
}

// func ReadLn(conn net.Conn, delim byte) (string, error) {
// 	reader := bufio.NewReader(conn)
// 	var buffer bytes.Buffer
// 	for {
// 		ba, isPrefix, err := reader.ReadLine()
// 		if err != nil {
// 			if err == io.EOF {
// 				break
// 			}
// 			return "", err
// 		}
// 		buffer.Write(ba)
// 		if !isPrefix {
// 			break
// 		}
// 	}
// 	return buffer.String(), nil
// }

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
