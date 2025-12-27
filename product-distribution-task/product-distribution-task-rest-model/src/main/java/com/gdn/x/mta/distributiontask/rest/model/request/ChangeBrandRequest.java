package com.gdn.x.mta.distributiontask.rest.model.request;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude
public class ChangeBrandRequest implements Serializable {
  private static final long serialVersionUID = -8507584957401368751L;
  private String productCode;
  private String brandCode;
  private String brandName;
}
