package com.gdn.x.mta.distributiontask.model.dto;

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
@JsonIgnoreProperties(ignoreUnknown = true)
public class BrandReport implements Serializable {
  @Serial
  private static final long serialVersionUID = -1247109163136822433L;
  private Date reportDate;
  private String reporter;
  private String reporterName;
  private String reporterEmail;
  private String reporterPhone;
  private String reporterAddress;
  private String reporterReason;
}
