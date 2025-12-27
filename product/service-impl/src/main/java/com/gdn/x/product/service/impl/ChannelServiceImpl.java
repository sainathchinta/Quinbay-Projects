package com.gdn.x.product.service.impl;

import java.util.Arrays;
import java.util.List;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.gdn.client_sdk.shade.org.apache.commons.lang3.StringUtils;
import com.gdn.x.product.service.api.ChannelService;

@Service
public class ChannelServiceImpl implements ChannelService {

  private static final String REGEX = ";";

  @Value("${channel.default.value}")
  private String channelDefaultValue;

  @Value("${channel.cnc.value}")
  private String channelCncValue;

  @Value("${channel.b2b.value}")
  private String channelB2BValue;

  @Value("${channel.list}")
  private String channelList;

  @Override
  public String getDefaultChannel() {
    return this.channelDefaultValue;
  }

  @Override
  public String getCncChannel() {
    return this.channelCncValue;
  }

  @Override
  public String getB2BChannel() {
    return this.channelB2BValue;
  }

  @Override
  public List<String> getListOfChannel() {
    return Arrays.asList(this.channelList.split(ChannelServiceImpl.REGEX));
  }

  @Override
  public boolean isValidChannel(String channelName) {
    return StringUtils.contains(this.channelList, channelName);
  }


}
