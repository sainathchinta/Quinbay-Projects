package com.gda.mta.product.dto;

import java.io.Serializable;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = false)
public class ItemNeedRevisionNotes implements Serializable {
  private static final long serialVersionUID = -5466213316108523913L;

  private String itemSku;
  private String skuCode;
  private Integer itemNumber;
  private String itemName;
  private List<String> vendorNotes;
  private List<String> vendorErrorFields;
}
