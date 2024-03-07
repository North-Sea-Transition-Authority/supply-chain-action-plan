package uk.co.nstauthority.scap.util;

import java.net.InetSocketAddress;
import java.net.Proxy;

public class ProxyUtil {
  public static Proxy createProxy(String proxyHost, String proxyPort) {
    Proxy proxy;
    if (proxyHost == null) {
      proxy = Proxy.NO_PROXY;
    } else {
      proxy = new Proxy(Proxy.Type.HTTP, new InetSocketAddress(proxyHost, Integer.parseInt(proxyPort)));
    }

    return proxy;
  }
}
