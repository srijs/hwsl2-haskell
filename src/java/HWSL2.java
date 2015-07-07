import java.nio.ByteBuffer;
import java.nio.charset.*;

public class HWSL2 {

  static {
    System.load("/Users/sreis/Repositories/hwsl2/src/java/hwsl2.so");
  }
  private static native boolean valid(ByteBuffer a);
  private static native boolean eq(ByteBuffer a, ByteBuffer b);
  private static native int cmp(ByteBuffer a, ByteBuffer b);
  private static native void unit(ByteBuffer a);
  private static native void mulBufRight(ByteBuffer a, ByteBuffer buf, long n);
  private static native void mulBufLeft(ByteBuffer a, ByteBuffer buf, long n);
  private static native void mul(ByteBuffer z, ByteBuffer a, ByteBuffer b);
  private static native void serialize(ByteBuffer a, ByteBuffer buf);
  private static native void unserialize(ByteBuffer a, ByteBuffer buf);

  private ByteBuffer buf;

  public HWSL2() {
    buf = ByteBuffer.allocateDirect(64);
  }

  public void reset() {
    unit(buf);
  }

  public boolean valid() {
    return valid(buf);
  }

  public boolean eq(HWSL2 b) {
    return eq(buf, b.buf);
  }

  public int cmp(HWSL2 b) {
    return cmp(buf, b.buf);
  }

  public void append(ByteBuffer b, long n) {
    mulBufRight(buf, b, n);
  }

  public void prepend(ByteBuffer b, long n) {
    mulBufLeft(buf, b, n);
  }

  public HWSL2 mul(HWSL2 b) {
    HWSL2 z = new HWSL2();
    mul(z.buf, buf, b.buf);
    return z;
  }

  public String serialize() {
    byte[] bytes = new byte[86];
    ByteBuffer strbuf = ByteBuffer.allocateDirect(86);
    serialize(buf, strbuf);
    strbuf.get(bytes);
    return new String(bytes, StandardCharsets.US_ASCII);
  }

  public void unserialize(String str) {
    byte[] bytes = str.getBytes(StandardCharsets.US_ASCII);
    ByteBuffer strbuf = ByteBuffer.allocateDirect(86);
    strbuf.put(bytes);
    unserialize(buf, strbuf);
  }

}
