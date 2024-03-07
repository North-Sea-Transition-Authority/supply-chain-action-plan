package uk.co.nstauthority.scap.util;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import uk.co.nstauthority.scap.error.exception.IllegalUtilClassInstantiationException;

public class StreamUtils {

  private StreamUtils() {
    throw new IllegalUtilClassInstantiationException(StreamUtils.class);
  }

  public static <T, K, U> Collector<T, ?, Map<K, U>> toLinkedHashMap(Function<? super T, ? extends K> keyMapper,
                                                                     Function<? super T, ? extends U> valueMapper) {

    return Collectors.toMap(
        keyMapper,
        valueMapper,
        (u, v) -> {
          throw new IllegalStateException(String.format("Duplicate key %s", u));
        },
        LinkedHashMap::new
    );
  }

  public static <T> Predicate<T> distinctByKey(Function<? super T, ?> keyExtractor) {
    Set<Object> seen = ConcurrentHashMap.newKeySet();
    return t -> seen.add(keyExtractor.apply(t));
  }

}
