package com.gdn.aggregate.platform.module.product.app.logging;

import ch.qos.logback.classic.pattern.ThrowableProxyConverter;
import ch.qos.logback.classic.spi.IThrowableProxy;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * @author priagung.satyagama
 * created at 5/21/2021
 */
public class SingleLineErrorConversionRule extends ThrowableProxyConverter {

  public static final String LINEBREAK = " ~$linebreak$~ ";

  private Pattern pattern;

  private String replacement;

  public SingleLineErrorConversionRule() {
    pattern = Pattern.compile("\r\n|\n\r|\n|\r");
    replacement = Matcher.quoteReplacement(LINEBREAK);
  }

  @Override
  protected String throwableProxyToString(IThrowableProxy tp) {
    return ": " + pattern.matcher(super.throwableProxyToString(tp).trim())
      .replaceAll(replacement);
  }

}
