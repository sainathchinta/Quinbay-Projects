package com.gdn.mta.bulk.models;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.Date;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude
@JsonIgnoreProperties(ignoreUnknown = true)
public class BrandReport implements Serializable {
  private static final long serialVersionUID = -1061138128720493736L;
  private Date reportDate;
  private String reporter;
  private String reporterName;
  private String reporterEmail;
  private String reporterPhone;
  private String reporterAddress;
  private String reporterReason;
}
