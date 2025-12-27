package com.gdn.x.product.service.task;

import java.util.List;
import java.util.Objects;
import java.util.concurrent.Callable;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.gdn.x.product.dao.api.PristineItemRepository;
import com.gdn.x.product.exception.PristineDataItemNotFoundException;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.PristineDataItem;
import com.gdn.x.product.model.entity.SalesCategorySequence;
import com.gdn.x.product.model.vo.ItemCatalogVO;
import com.gdn.x.product.service.api.CacheEvictHelperService;
import com.gdn.x.product.service.api.ItemService;
import com.gdn.x.product.service.api.SaveAndPublishService;

/**
 * Created by Vishal on 22/09/17.
 */
public class SalesCatalogUpdateForPristineTask implements Callable<Boolean> {

  private static final Logger LOGGER =
      LoggerFactory.getLogger(SalesCatalogUpdateForPristineTask.class);

  private List<String> pristineIds;
  private ItemService itemService;
  private PristineItemRepository pristineItemRepository;
  private String storeId;
  private CacheEvictHelperService cacheEvictHelperService;
  private SaveAndPublishService saveAndPublishService;

  private static final String PRISTINE_NOT_FOUND = "PristineDataItem not found";

  public SalesCatalogUpdateForPristineTask(List<String> pristineIds, ItemService itemService,
     PristineItemRepository pristineItemRepository, String storeId,
      CacheEvictHelperService cacheEvictHelperService,
     SaveAndPublishService saveAndPublishService) {
    super();
    this.pristineIds = pristineIds;
    this.itemService = itemService;
    this.pristineItemRepository = pristineItemRepository;
    this.storeId = storeId;
    this.cacheEvictHelperService = cacheEvictHelperService;
    this.saveAndPublishService = saveAndPublishService;
  }

  @Override
  public Boolean call() {
    Boolean isSuccess = Boolean.TRUE;
    for (String pristineId : pristineIds) {
      try {
        LOGGER.info("trying to update sales catalog for pristine Id : {}", pristineId);
        List<ItemCatalogVO> itemCatalogVOs = itemService.getItemCatalogsByPristineId(pristineId);
        List<SalesCategorySequence> salesCategorySequenceDTOList =
            itemService.getSalesCategorySequenceListFromCategoryHierarchy(itemCatalogVOs);
        PristineDataItem pristineDataItem = pristineItemRepository.findByPristineId(pristineId);
        if(Objects.isNull(pristineDataItem)){
          throw new PristineDataItemNotFoundException(PRISTINE_NOT_FOUND);
        }
        pristineDataItem.setPristineCategoriesHierarchy(itemCatalogVOs);
        pristineDataItem.setSalesCategorySequences(salesCategorySequenceDTOList);
        pristineItemRepository.save(pristineDataItem);
        List<Item> items = itemService.getItemsByPristineId(storeId, pristineId);
        saveAndPublishService.publishListOfItems(items);
        cacheEvictHelperService.evictPristineItemCache(storeId, pristineDataItem, items);
      } catch (Exception e) {
        LOGGER.error("failed to update sales catalog for pristineId : {}, stackTrace: {}",
            pristineId, e);
        isSuccess = Boolean.FALSE;
      }
    }
    return isSuccess;
  }

}
