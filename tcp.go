package tcp

import (
	"net"
	"go.k6.io/k6/js/modules"
	// "io"
	// "bufio"
)

func init() {
	modules.Register("k6/x/tcp", new(TCP))
}

type TCP struct{}

func (tcp *TCP) Connect(addr string) (net.Conn, error) {
	conn, err := net.Dial("tcp", addr)
	if err != nil {
		return nil, err
	}

	return conn, nil
}

func (tcp *TCP) Write(conn net.Conn, data []byte) error {
	_, err := conn.Write(data)
	if err != nil {
		return err
	}

	return nil
}

func (tcp *TCP) Read(conn net.Conn, size int) ([]byte, error) {
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

func (tcp *TCP) WriteLn(conn net.Conn, data []byte, delim []byte) error {
	return tcp.Write(conn, append(data, delim...))
}

func (tcp *TCP) Close(conn net.Conn) error {
	err := conn.Close()
	if err != nil {
		return err
	}
	return nil
}

func (tcp *TCP) CloseWrite(conn net.Conn) error {
	err := conn.CloseWrite()
	if err != nil {
		return err
	}
	return nil
}

func (tcp *TCP) CloseRead(conn net.Conn) error {
	err := conn.CloseRead()
	if err != nil {
		return err
	}
	return nil
}
