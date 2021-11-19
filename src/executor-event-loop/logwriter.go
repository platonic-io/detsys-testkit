package executorEL

type LogWriter [][]byte

func newLogWriter() *LogWriter {
	x := LogWriter(make([][]byte, 0, 0))
	return &x
}

func (lw *LogWriter) dump() []string {
	logs := make([]string, 0, len(*lw))
	for _, l := range *lw {
		logs = append(logs, string(l))
	}
	*lw = LogWriter(make([][]byte, 0, 0))
	return logs
}

func (_ *LogWriter) Sync() error {
	return nil
}

func (lw *LogWriter) Write(p []byte) (n int, err error) {
	if len(p) > 0 {
		// make a copy of the line so that it doesn't get changed later
		line := make([]byte, len(p))
		copy(line, p)

		// we remove last byte since it is an newline
		*lw = append(*lw, line[:len(line)-1])
	}
	return len(p), nil
}

/*
func (lw LogWriter) AppendToLogger(logger *zap.Logger) *zap.Logger {
	return logger.WithOptions(zap.WrapCore(func(c zapcore.Core) zapcore.Core {
		config := zap.NewDevelopmentEncoderConfig()
		config.TimeKey = ""
		config.LineEnding = ""
		dbLogger := zapcore.NewCore(zapcore.NewConsoleEncoder(config), lw, c)
		return zapcore.NewTee(c, dbLogger)
	}))
}
*/
