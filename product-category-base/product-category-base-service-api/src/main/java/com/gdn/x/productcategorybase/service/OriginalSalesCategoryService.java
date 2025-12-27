package com.gdn.x.productcategorybase.service;

import java.util.List;

import com.gdn.x.productcategorybase.dto.OscInfoUpdateDTO;
import com.gdn.x.productcategorybase.dto.response.OriginalSalesCategoryResponse;
import com.gdn.x.productcategorybase.dto.response.OscSummaryResponse;
import com.gdn.x.productcategorybase.entity.OriginalSalesCategory;

public interface OriginalSalesCategoryService {

  /**
   * filter osc list on name /code/activated flag
   *
   * @param oscCode
   * @param activated
   * @param keyword
   * @return
   */
  List<OscSummaryResponse> filterSummaryOSC(String oscCode, String keyword, Boolean activated);

  /**
   * Api to save original sales category
   *
   * @param entity
   */
  String save(OriginalSalesCategory entity) throws Exception;

  /**
   * Api to fetch activated original sales category by store ID and ID.
   *
   * @param storeId
   * @param id
   */
  OriginalSalesCategory findByStoreIdAndIdAndMarkForDeleteFalseAndActivatedTrue(String storeId, String id);

  /**
   * Api to update original sales category by store ID and ID.
   *
   * @param storeId
   * @param userName
   * @param oscInfoUpdateDTO
   */
  void updateOsc(String storeId, String userName, OscInfoUpdateDTO oscInfoUpdateDTO) throws Exception;


  /**
   * Api to fetch original sales category by store ID and ID.
   *
   * @param storeId
   * @param id
   */
  OriginalSalesCategoryResponse findByStoreIdAndIdAndMarkForDeleteFalse(String storeId, String id);
}
