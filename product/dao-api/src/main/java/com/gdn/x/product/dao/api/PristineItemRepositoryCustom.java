package com.gdn.x.product.dao.api;

import java.util.List;
import java.util.Set;
import com.gdn.x.product.model.entity.PristineDataItem;

/**
 * Created by keshashah on 18/12/17.
 */
public interface PristineItemRepositoryCustom {

  /**
   * get all distinct pristineIds
   * @return
   */
  List<String> getAllPristineIds();

  /**
   * Update Sales categorySequences and DPC
   * @param pristineDataItems
   */
  void updateSalesCategorySequencesAndDPC(Set<PristineDataItem> pristineDataItems);

  /**
   * Update Pristine DPC
   * @param pristineDataItems
   */
  void updatePristineMasterDPC(PristineDataItem pristineDataItems);

  /**
   * Update Pristine DPC
   * @param pristineDataItems
   */
  void updatePristineDPC(List<PristineDataItem> pristineDataItems);

}
