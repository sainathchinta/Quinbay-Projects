package com.gdn.x.product.service.properties;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude()
public class MandatoryParameter {
  private HeaderKey headerKey = new HeaderKey();
  private String storeId;
  private String channelId;
  private String clientId;
  private String requestId;
  private String username;

  @Data
  @Builder
  @AllArgsConstructor
  @NoArgsConstructor
  public static class HeaderKey {

    private String storeId = "storeId";

    private String storeIdDefaultValue = "storeId";

    private String clientId = "clientId";

    private String clientIdDefaultValue = "clientId";

    private String channelId = "channelId";

    private String channelIdDefaultValue = "channelId";

    private String username = "username";

    private String usernameDefaultValue = "username";

    private String requestId = "requestId";

    private String requestIdDefaultValue = "requestId";

  }

}
