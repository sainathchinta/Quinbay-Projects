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
public class UpsertOfflineItemFailedResponse implements Serializable {

  private static final long serialVersionUID = 7633158259435685697L;

  private String itemSku;
  private String pickupPointCode;
  private String errorCode;
}
