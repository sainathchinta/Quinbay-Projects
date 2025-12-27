package com.gdn.mta.domain.event.modal;

import java.io.Serializable;
import java.util.List;

import com.gdn.common.base.entity.GdnBaseDomainEventModel;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class SolrReviewProductCollectionDeleteEvent extends GdnBaseDomainEventModel implements Serializable {

  private static final long serialVersionUID = -5527546513029846516L;

  private List<String> ids;
}
