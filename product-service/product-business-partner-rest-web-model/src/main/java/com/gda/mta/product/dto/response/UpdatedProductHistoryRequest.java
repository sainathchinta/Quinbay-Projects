package com.gda.mta.product.dto.response;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import com.gdn.mta.product.entity.UpdatedProductHistory;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@JsonIgnoreProperties(ignoreUnknown = true)
@Data
@AllArgsConstructor
@NoArgsConstructor
public class UpdatedProductHistoryRequest extends GdnBaseDomainEventModel implements Serializable {
  private static final long serialVersionUID = 1594230688245453588L;

  private List<UpdatedProductHistory> updatedProductHistories = new ArrayList<>();

}
