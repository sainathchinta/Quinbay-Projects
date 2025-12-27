package com.gdn.x.product.service.api;

import java.util.List;

public interface ChannelService {
  String getDefaultChannel();

  String getCncChannel();

  String getB2BChannel();

  List<String> getListOfChannel();

  boolean isValidChannel(String channelName);
}
