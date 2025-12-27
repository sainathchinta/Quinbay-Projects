package com.gdn.x.product.rest.web.model.request;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductLevel3SummaryRequest implements Serializable {

  private static final long serialVersionUID = 3691287029303425442L;
  private List<String> productSkuList = new ArrayList<>();
}
