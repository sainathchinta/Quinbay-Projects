package com.gdn.x.productcategorybase.dto.request;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.dto.MerchantConfigurationRequest;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class MerchantConfigurationRequestList implements Serializable {

  private static final long serialVersionUID = 7957122359657959966L;
  private List<MerchantConfigurationRequest> merchantConfigurationRequestList = new ArrayList<>();
}
