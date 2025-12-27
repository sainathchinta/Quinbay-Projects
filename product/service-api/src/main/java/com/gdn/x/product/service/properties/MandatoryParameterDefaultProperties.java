package com.gdn.x.product.service.properties;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;

import org.springframework.stereotype.Component;


@Builder
@RequiredArgsConstructor
@AllArgsConstructor
@Data
@Component
public class MandatoryParameterDefaultProperties {

  @Value("${mandatory.parameter.default.storeId:10001}")
  private String storeId;
  @Value("${mandatory.parameter.default.clientId:client}")
  private String clientId;
  @Value("${mandatory.parameter.default.channelId:channel}")
  private String channelId;
  @Value("${mandatory.parameter.default.username:user}")
  private String username;
  @Value("${mandatory.parameter.default.username:requestId}")
  private String requestId;
}
