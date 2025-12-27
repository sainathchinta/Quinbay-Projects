package com.gdn.mta.bulk.models.download;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.io.Serializable;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude
@JsonIgnoreProperties(ignoreUnknown = true)
public class IPRProductsDownloadRequest extends BulkDownloadRequest {

  private static final long serialVersionUID = -3085392060586400285L;
  private String keyword;
  private String timeFilterWebType;
  private String state;
  private String businessPartnerCode;
  private String categoryCode;
  private String brandCode;
  private String sortOrder;
  private String assignedTo;
  private Boolean assigned;
}