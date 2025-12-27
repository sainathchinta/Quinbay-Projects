package com.gdn.mta.bulk.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class BulkCncUpsertErrorDTO implements Serializable {

  private static final long serialVersionUID = -7097926703529127783L;

  private String itemSku;
  private String pickupPointCode;
  private String reason;
}
