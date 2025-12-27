package com.gdn.x.productcategorybase.repository;

import com.gdn.x.productcategorybase.dto.SizeChartFilterRequestDTO;
import com.gdn.x.productcategorybase.entity.SizeChart;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import java.util.List;

public interface SizeChartRepositoryCustom {

  /**
   * fetch list of size charts based on filter and sort accordingly
   * @param storeId
   * @param filter
   * @param pageable
   * @param sortExternalSizeChartsByCreatedDateDescending
   * @return
   */
  Page<SizeChart> findByStoreIdAndFilterAppliedAndBusinessPartnerCodeAndMarkForDeleteFalse(
      String storeId, SizeChartFilterRequestDTO filter, Pageable pageable,
      boolean sortExternalSizeChartsByCreatedDateDescending);
}