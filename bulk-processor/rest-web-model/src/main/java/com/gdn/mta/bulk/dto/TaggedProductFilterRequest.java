package com.gdn.mta.bulk.dto;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.util.List;
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class TaggedProductFilterRequest {
  private static final long serialVersionUID = 8123667022442841999L;
  private List<String> businessPartnerCodes;
  private List<String> categoryCodes;
  private List<String> productTypes;
  private List<String> itemSkus;
  private String emailAddress;
}