package com.gdn.x.productcategorybase.domain.event.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.List;

@JsonIgnoreProperties(ignoreUnknown = true)
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class SolrAddBatchPcbProductDomainEventModel extends GdnBaseDomainEventModel implements
    Serializable {
  private static final long serialVersionUID = -3773168176476529991L;
  List<SolrAddPcbProductDomainEventModel> productDomainEventModelList;
}
