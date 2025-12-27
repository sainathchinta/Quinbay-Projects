package com.gdn.x.product.dao.api;

import java.util.List;
import java.util.Set;
import org.springframework.data.mongodb.repository.MongoRepository;
import com.gdn.x.product.model.entity.PristineDataItem;

/**
 * Created by keshashah on 15/12/17.
 */
public interface PristineItemRepository
    extends MongoRepository<PristineDataItem, String>, PristineItemRepositoryCustom {

  /**
   * get PristineDataItem by pristineId
   * @param pristineId
   * @return
   */
  PristineDataItem findByPristineId(String pristineId);

  /**
   * get list of PristineDataItem by pristineIds
   * @param pristineIdList
   * @return
   */
  List<PristineDataItem> findByPristineIdIn(Set<String> pristineIdList);

  /**
   * get list of PristineDataItem by pristineMasterIds
   * @param pristineMasterIds
   * @return
   */
  List<PristineDataItem> findByPristineMasterIdIn(Set<String> pristineMasterIds);

  /**
   * get list of PristineDataItem by pristineMasterId and pristineId not present
   * @param pristineMasterId
   * @param productCondition
   * @param pristineId
   * @return
   */
  List<PristineDataItem> findByPristineMasterIdAndProductConditionAndPristineIdNot(
      String pristineMasterId, String productCondition, String pristineId);

  /**
   * get list of PristineDataItem by pristineMasterId and pristineId not present
   * @param pristineMasterId
   * @param pristineId
   * @return
   */
  List<PristineDataItem> findByPristineMasterIdAndPristineIdNot(String pristineMasterId, String pristineId);

  /**
   * get list of PristineDataItem by pristineMasterId
   * @param pristineMasterId
   * @return
   */
  List<PristineDataItem> findByPristineMasterId(String pristineMasterId);

  /**
   *
   * @param pristineProductName
   * @return
   */
  PristineDataItem findByPristineProductNameIgnoreCase(String pristineProductName);

}
