package uk.co.nstauthority.scap.audit;


import uk.co.nstauthority.scap.authentication.ServiceUserDetail;

public class AuditRevisionUtil {

  private static final ThreadLocal<ServiceUserDetail> fallbackAuditUser = new ThreadLocal<>();

  private AuditRevisionUtil() {
  }

  public static ServiceUserDetail getFallbackAuditUser() {
    return fallbackAuditUser.get();
  }

  public static void withFallbackAuditUser(ServiceUserDetail user, Runnable runnable) {
    fallbackAuditUser.set(user);
    try {
      runnable.run();
    } finally {
      fallbackAuditUser.remove();
    }
  }
}