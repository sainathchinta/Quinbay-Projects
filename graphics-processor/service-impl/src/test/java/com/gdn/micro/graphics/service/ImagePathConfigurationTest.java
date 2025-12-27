package com.gdn.micro.graphics.service;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.common.exception.ApplicationRuntimeException;

public class ImagePathConfigurationTest {

  private static final String CLIENT = "client";
  private static final String CLIENT_2 = "client2";
  private static final String CLIENT_3 = "client3";
  private static final String XGP = "XGP";

  @InjectMocks
  private ImagePathConfiguration imagePathConfiguration;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    ReflectionTestUtils.setField(imagePathConfiguration, "imagePathConfigurationMapping", "client=XGP, client2= ");
  }

  @Test
  void getLocationPrefixTest() {
    String result = imagePathConfiguration.getLocationPrefix(CLIENT);
    Assertions.assertEquals(XGP, result);
  }

  @Test
  void getLocationPrefixTestError() {
    try {
      String result = imagePathConfiguration.getLocationPrefix(CLIENT_2);
    } catch (ApplicationRuntimeException ex){
      Assertions.assertNotNull(ex);
    }
  }

  @Test
  void getLocationPrefixTestNullError() {
    try {
      String result = imagePathConfiguration.getLocationPrefix(CLIENT_3);
    } catch (ApplicationRuntimeException ex){
      Assertions.assertNotNull(ex);
    }
  }
}