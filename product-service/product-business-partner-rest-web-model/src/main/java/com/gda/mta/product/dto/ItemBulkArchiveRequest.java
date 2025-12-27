package com.gda.mta.product.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
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
public class ItemBulkArchiveRequest extends BaseRequest {

  private static final long serialVersionUID = -2278864757396684021L;

  private String businessPartnerCode;
  private List<String> itemSkus;
}
