package uk.co.nstauthority.scap.jooq;

import java.io.Serial;
import org.jooq.ExecuteContext;
import org.jooq.ExecuteListener;
import org.springframework.stereotype.Component;

@Component
public class JooqStatisticsListener implements ExecuteListener {

  @Serial
  private static final long serialVersionUID = 7399239846062763212L;

  public final transient ThreadLocal<Integer> count = ThreadLocal.withInitial(() -> 0);

  public Integer getCount() {
    return count.get();
  }

  public void clear() {
    count.remove();
  }

  @Override
  public void start(ExecuteContext ctx) {
    count.set(count.get() + 1);
  }

  // spring 3.x all the below methods default & none of these are needed
  @Override
  public void renderStart(ExecuteContext ctx) {

  }

  @Override
  public void renderEnd(ExecuteContext ctx) {

  }

  @Override
  public void prepareStart(ExecuteContext ctx) {

  }

  @Override
  public void prepareEnd(ExecuteContext ctx) {

  }

  @Override
  public void bindStart(ExecuteContext ctx) {

  }

  @Override
  public void bindEnd(ExecuteContext ctx) {

  }

  @Override
  public void executeStart(ExecuteContext ctx) {

  }

  @Override
  public void executeEnd(ExecuteContext ctx) {

  }

  @Override
  public void outStart(ExecuteContext ctx) {

  }

  @Override
  public void outEnd(ExecuteContext ctx) {

  }

  @Override
  public void fetchStart(ExecuteContext ctx) {

  }

  @Override
  public void resultStart(ExecuteContext ctx) {

  }

  @Override
  public void recordStart(ExecuteContext ctx) {

  }

  @Override
  public void recordEnd(ExecuteContext ctx) {

  }

  @Override
  public void resultEnd(ExecuteContext ctx) {

  }

  @Override
  public void fetchEnd(ExecuteContext ctx) {

  }

  @Override
  public void end(ExecuteContext ctx) {

  }

  @Override
  public void exception(ExecuteContext ctx) {

  }

  @Override
  public void warning(ExecuteContext ctx) {

  }

}
