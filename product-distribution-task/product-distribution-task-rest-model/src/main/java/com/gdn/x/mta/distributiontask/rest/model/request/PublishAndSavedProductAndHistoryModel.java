package com.gdn.x.mta.distributiontask.rest.model.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.mta.distributiontask.domain.event.InternalHistoryEventModel;
import com.gdn.x.mta.distributiontask.model.Product;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
@Builder
public class PublishAndSavedProductAndHistoryModel {
  private Product savedProduct;
  private InternalHistoryEventModel internalHistoryEventModel;
  private Boolean doPublish;
}
