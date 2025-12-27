package com.gda.mta.product.dto.response;

import java.io.Serializable;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gda.mta.product.dto.ProductDataAutoFixHistoryDto;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@JsonIgnoreProperties(ignoreUnknown = true)
@Data
@AllArgsConstructor
@NoArgsConstructor
@ToString
public class ProductDataAutoFixHistoryListRequest extends GdnBaseDomainEventModel implements Serializable {

  private List<ProductDataAutoFixHistoryDto> productDataAutoFixHistoryDtoList;
}
