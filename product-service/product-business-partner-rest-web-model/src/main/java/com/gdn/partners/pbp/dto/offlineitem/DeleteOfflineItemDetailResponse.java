package com.gdn.partners.pbp.dto.offlineitem;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class DeleteOfflineItemDetailResponse implements Serializable {

  private static final long serialVersionUID = 2606629421510551249L;

  private String itemSku;
  private String pickupPointCode;
  private String errorMessage;

}
