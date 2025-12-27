package com.gdn.x.product.service.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.MockitoAnnotations.openMocks;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.springframework.test.util.ReflectionTestUtils;


public class ChannelServiceImplTest {

  private static final String DEFAULT = "DEFAULT";
  private static final String CNC = "CNC";

  @InjectMocks
  private ChannelServiceImpl channelServiceImpl;

  @Test
  public void getChannelDefaultValue() {
    assertEquals(this.channelServiceImpl.getDefaultChannel(), ChannelServiceImplTest.DEFAULT);
  }

  @Test
  public void getChannelCncValue() {
    assertEquals(this.channelServiceImpl.getCncChannel(), ChannelServiceImplTest.CNC);
  }

  @Test
  public void getListOfChannel() {
    assertEquals(this.channelServiceImpl.getListOfChannel().size(), 4);
  }

  @BeforeEach
  public void init() {
    openMocks(this);
    ReflectionTestUtils.setField(this.channelServiceImpl, "channelDefaultValue",
        ChannelServiceImplTest.DEFAULT, String.class);
    ReflectionTestUtils.setField(this.channelServiceImpl, "channelCncValue",
        ChannelServiceImplTest.CNC, String.class);
    ReflectionTestUtils.setField(this.channelServiceImpl, "channelList",
        "web;web-pulsa;mobile;mobile-pulsa", String.class);
  }

  @Test
  public void isValidChannel() {
    assertTrue(this.channelServiceImpl.isValidChannel("web"));
  }

  @Test
  public void isValidChannelWithNullValue() {
    assertFalse(this.channelServiceImpl.isValidChannel(null));
  }

  @AfterEach
  public void tearDown() {}

  @Test
  public void getCncChannelValue() {
    assertEquals(this.channelServiceImpl.getCncChannel(), ChannelServiceImplTest.CNC);
  }
}
