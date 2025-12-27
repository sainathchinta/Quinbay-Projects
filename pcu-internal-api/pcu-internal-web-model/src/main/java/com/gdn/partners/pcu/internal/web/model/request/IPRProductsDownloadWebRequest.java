package com.gdn.partners.pcu.internal.web.model.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serial;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

@AllArgsConstructor
@NoArgsConstructor
@Data
@Builder
@JsonInclude
@JsonIgnoreProperties(ignoreUnknown = true)
public class IPRProductsDownloadWebRequest implements Serializable {
  @Serial
  private static final long serialVersionUID = 1204433948021929231L;
  private String keyword;
  private String timeFilterWebType;
  private String state;
  private String assignedTo;
  private String businessPartnerCode;
  private String categoryCode;
  private String brandCode;
  private String sortOrder;
  private Boolean assigned;
  private List<String> productSkuList = new ArrayList<>();
}
