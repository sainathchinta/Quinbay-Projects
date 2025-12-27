package com.gdn.x.productcategorybase.domain.event.model;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class RestrictedKeywordHistoryEventModel extends GdnBaseDomainEventModel implements Serializable {
  private static final long serialVersionUID = 8718622891457150057L;
  private String storeId;
  private String userName;
  private String keywordId;
  private List<RestrictedKeywordActivityHistory> restrictedKeywordActivityHistoryList = new ArrayList<>();
}
