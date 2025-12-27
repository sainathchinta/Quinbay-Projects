package com.gdn.partners.pcu.internal.client.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serial;
import java.io.Serializable;
import java.util.Date;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude
public class BrandReportDetailResponse implements Serializable {
  @Serial
  private static final long serialVersionUID = 8918070835961084892L;
  private Date reportDate;
  private String brandName;
  private String reporterName;
  private String reporterEmail;
  private String reporterReason;
}
