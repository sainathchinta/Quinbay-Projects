package com.gdn.x.product.service.cache;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Set;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Service;

import com.gdn.common.exception.ApplicationException;
import com.gdn.x.product.constants.ErrorMessages;
import com.gdn.x.product.dao.api.PristineItemRepository;
import com.gdn.x.product.enums.CacheNames;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.PristineDataItem;
import com.gdn.x.product.model.vo.CacheItemVO;
import com.gdn.x.product.model.vo.PristineItemAndSiblingsVO;
import com.gdn.x.product.service.api.CacheItemHelperService;
import com.gdn.x.product.service.api.ItemService;
import com.gdn.x.product.service.api.PristineCacheableService;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class PristineCacheableServiceImpl implements PristineCacheableService {

  @Autowired
  private PristineItemRepository pristineItemRepository;

  @Autowired
  @Qualifier("redisTemplate")
  private RedisTemplate redisTemplate;

  @Autowired
  private CacheItemHelperService cacheItemHelperService;

  @Autowired
  @Lazy
  private ItemService itemService;

  @Autowired
  private ApplicationContext applicationContext;

  private static final Logger LOGGER = LoggerFactory.getLogger(PristineCacheableServiceImpl.class);

  private static final String PM = "pm";

  private static final String PS = "ps";

  private static final String ITEM = "item";

  private static final String cacheKeySeparator = "-";

  private static final String cacheValueKeySeparator = ":";

  private static final String PRISTINE_ID_MUST_NOT_BE_BLANK = "pristineId must not be blank";

  private static final String STORE_ID_MUST_NOT_BE_BLANK = "storeId must not be blank";

  @Override
  public void evictPristineItemAndSiblingsCacheAndRebuild(String storeId, PristineDataItem pristineDataItem) {
    evictPristineItemCacheAndRebuildIfCacheExists(storeId, pristineDataItem.getPristineId());
    List<PristineDataItem> siblings = new ArrayList<>();
    if (StringUtils.isNotBlank(pristineDataItem.getProductCondition())) {
      pristineItemRepository
          .findByPristineMasterIdAndProductConditionAndPristineIdNot(pristineDataItem.getPristineMasterId(),
              pristineDataItem.getProductCondition(), pristineDataItem.getPristineId());
    } else {
      pristineItemRepository.findByPristineMasterIdAndPristineIdNot(pristineDataItem.getPristineMasterId(),
          pristineDataItem.getPristineId());
    }
    if(CollectionUtils.isNotEmpty(siblings)) {
      for (PristineDataItem sibling : siblings) {
        evictPristineItemCacheAndRebuildIfCacheExists(storeId, sibling.getPristineId());
      }
    }
  }

  private void evictPristineItemCacheAndRebuildIfCacheExists(String storeId, String pristineId){
    if(evictPristineItemCache(storeId, pristineId)){
      getPristineCacheableServiceBean().findPristineItemAndItsSiblingsByPristineId(storeId, pristineId);
    }
  }

  @Override
  public boolean evictPristineItemCache(String storeId, String pristineId) {
    String key = getPristineSiblingsCacheKey(storeId, pristineId);
    LOGGER.info("Cache eviction requested for pristine item with id:{}", pristineId);
    redisTemplate.delete(key);
    LOGGER.info("Cache eviction successful for pristine item with id:{}", pristineId);
    return true;
  }

  @Override
  @Cacheable(value = {
      CacheNames.FIND_PRISTINE_AND_ITS_SIBLINGS_BY_PRISTINE_ID}, key = "#storeId + '-' + 'pm' + '-' + 'ps' + '-' + #pristineId", unless = "#result == null")
  public PristineItemAndSiblingsVO findPristineItemAndItsSiblingsByPristineId(
      String storeId, String pristineId){
    return getPristineItemAndSiblingsVO(pristineId);
  }

  @Override
  public CacheItemVO findItemByPristine(String storeId, PristineDataItem pristineDataItem) {
    Set<String> itemSkus = cacheItemHelperService
        .findCacheableItemSkusByStoreIdAndPristineIdAndMarkForDeleteFalse(storeId,
            pristineDataItem.getPristineId());
    if (CollectionUtils.isNotEmpty(itemSkus)) {
      List<Item> itemList = itemService.findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalse(storeId, itemSkus);
      return new CacheItemVO(
          itemList.stream().filter(item -> StringUtils.isNotBlank(item.getItemCode())).findFirst()
              .map(filterItem -> filterItem.getItemCode()).orElse(null));
    }
    return null;
  }

  @Override
  public Item findFirstItemByPristine(PristineDataItem pristineDataItem) throws ApplicationException {
    Set<String> itemSkus = cacheItemHelperService
        .findCacheableItemSkusByStoreIdAndPristineIdAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, pristineDataItem.getPristineId());
    if (CollectionUtils.isNotEmpty(itemSkus)) {
      List<Item> itemList = itemService.findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, itemSkus);
      Item validItem =
          itemList.stream().filter(item -> StringUtils.isNotBlank(item.getItemCode())).findFirst().orElse(null);
      if (Objects.nonNull(validItem)) {
        return validItem;
      } else {
        if(CollectionUtils.isNotEmpty(itemList)) {
          return itemList.get(0);
        }
      }
    }
    log.warn(ErrorMessages.NO_VALID_ITEM_FOUND, pristineDataItem.getPristineId());
    return null;
  }


  private String getPristineSiblingsCacheKey(String storeId, String pristineId){
    StringBuilder pristineSiblingsCacheKey = new StringBuilder(
        CacheNames.FIND_PRISTINE_AND_ITS_SIBLINGS_BY_PRISTINE_ID);
    pristineSiblingsCacheKey.append(cacheValueKeySeparator)
        .append(storeId)
        .append(cacheKeySeparator)
        .append(PM)
        .append(cacheKeySeparator)
        .append(PS)
        .append(cacheKeySeparator)
        .append(pristineId);
    return pristineSiblingsCacheKey.toString();
  }

  private PristineItemAndSiblingsVO getPristineItemAndSiblingsVO(String pristineId) {
    PristineItemAndSiblingsVO pristineItemAndSiblingsVO = new PristineItemAndSiblingsVO();
    LOGGER.info("Cache missed for pristine item with id:{}", pristineId);
    PristineDataItem pristineDataItem = pristineItemRepository.findByPristineId(pristineId);
    pristineItemAndSiblingsVO.setPristineDataItem(pristineDataItem);
    if (pristineDataItem != null) {
      List<PristineDataItem> siblings = getSiblings(pristineDataItem);
      pristineItemAndSiblingsVO.setSiblings(siblings);
    }
    return pristineItemAndSiblingsVO;
  }

  private List<PristineDataItem> getSiblings(PristineDataItem pristineDataItem) {
    if (StringUtils.isNotBlank(pristineDataItem.getProductCondition())) {
      return pristineItemRepository
          .findByPristineMasterIdAndProductConditionAndPristineIdNot(pristineDataItem.getPristineMasterId(),
              pristineDataItem.getProductCondition(), pristineDataItem.getPristineId());
    } else {
      return pristineItemRepository.findByPristineMasterIdAndPristineIdNot(pristineDataItem.getPristineMasterId(),
          pristineDataItem.getPristineId());
    }
  }

  private PristineCacheableService getPristineCacheableServiceBean() {
    //It's used to invoke the aspect to build the cache
    return applicationContext.getBean(PristineCacheableService.class);
  }
}
