package com.gdn.partners.pcu.internal.streaming.model.bulk;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.io.Serial;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

@EqualsAndHashCode(callSuper = true)
@AllArgsConstructor
@NoArgsConstructor
@Data
@JsonInclude
@JsonIgnoreProperties(ignoreUnknown = true)
public class IPRProductsDownloadRequest extends BulkDownloadRequest implements Serializable {
  @Serial
  private static final long serialVersionUID = 5015282698887558417L;
  private String keyword;
  private String timeFilterWebType;
  private String state;
  private String businessPartnerCode;
  private String categoryCode;
  private String brandCode;
  private String sortOrder;
  private String assignedTo;
  private Boolean assigned;
  private List<String> productSkuList = new ArrayList<>();
}
