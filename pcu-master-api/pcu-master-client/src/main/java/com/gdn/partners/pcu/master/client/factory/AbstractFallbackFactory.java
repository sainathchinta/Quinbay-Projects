package com.gdn.partners.pcu.master.client.factory;

import com.gdn.partners.pcu.master.properties.ApplicationProperties;
import feign.hystrix.FallbackFactory;
import lombok.extern.slf4j.Slf4j;

/**
 * Abstract class for fallback factory
 *
 * @author Pradeep Reddy
 */
@Slf4j
public abstract class AbstractFallbackFactory<T> implements FallbackFactory<T> {

  private ApplicationProperties applicationProperties;

  public AbstractFallbackFactory(ApplicationProperties applicationProperties) {
    this.applicationProperties = applicationProperties;
  }

  @Override
  public T create(Throwable cause) {
    if (applicationProperties.isProduction()) {
      log.error("Error while invoking feign client", cause);
    } else {
      log.error("Error while invoking feign client: {}: {}", cause.getClass(), cause.getMessage());
    }
    return doCreate(cause);
  }

  /**
   * Do create fallback
   *
   * @param cause exception cause
   * @return fallback
   */
  public abstract T doCreate(Throwable cause);

  public ApplicationProperties getApplicationProperties() {
    return applicationProperties;
  }
}
