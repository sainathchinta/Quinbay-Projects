package com.gdn.mta.bulk.util;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;


public class ConverterUtilTest {

  @Test
  public void getEarlierDateBySeconds() {
    Assertions.assertTrue(ConverterUtil.getEarlierDateBySeconds(30).getTime()
        < ConverterUtil.getEarlierDateBySeconds(0).getTime());
  }
}