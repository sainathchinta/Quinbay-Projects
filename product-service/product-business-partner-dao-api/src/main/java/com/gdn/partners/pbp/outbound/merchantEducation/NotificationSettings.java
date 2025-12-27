package com.gdn.partners.pbp.outbound.merchantEducation;

import java.io.Serializable;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@JsonIgnoreProperties(ignoreUnknown = true)
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class NotificationSettings implements Serializable {

  private String username;
  private String storeCode;
  private Map<String, Map<String, Boolean>> notificationSettings;
}
