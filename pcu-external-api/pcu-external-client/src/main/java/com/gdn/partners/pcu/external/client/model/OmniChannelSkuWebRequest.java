package com.gdn.partners.pcu.external.client.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

@Data
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class OmniChannelSkuWebRequest implements Serializable {
  private String sellerCode;
  private List<String> omniChannelSkus = new ArrayList<>();
}
