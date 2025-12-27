package com.gdn.mta.bulk.models.download;

import java.io.Serializable;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@JsonIgnoreProperties(ignoreUnknown = true)
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class BulkPriceRecommendationDownloadRequest  extends BulkDownloadRequest implements Serializable {

  private static final long serialVersionUID = -6722594555631167698L;

  private Set<String> businessPartnerCodes;
  private Set<String> categoryCodes;
  private Set<String> productTypes;
  private Set<String> brandNames;
}
