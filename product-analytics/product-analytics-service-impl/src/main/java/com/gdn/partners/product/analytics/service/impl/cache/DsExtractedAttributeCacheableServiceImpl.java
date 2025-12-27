package com.gdn.partners.product.analytics.service.impl.cache;

import com.gdn.partners.product.analytics.entity.DSExtractionEntity;
import com.gdn.partners.product.analytics.model.CacheNames;
import com.gdn.partners.product.analytics.model.RedisConstants;
import com.gdn.partners.product.analytics.repository.DsExtractionAttributesRepository;
import com.gdn.partners.product.analytics.service.cache.DsExtractedAttributeCacheableService;
import lombok.RequiredArgsConstructor;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.stereotype.Service;

/**
 * to clear DS EXTRACTED attribute cache
 */
@Service
@RequiredArgsConstructor
public class DsExtractedAttributeCacheableServiceImpl
    implements DsExtractedAttributeCacheableService {

  private final DsExtractionAttributesRepository dsExtractionAttributesRepository;

  @Override
  @CacheEvict(cacheManager = RedisConstants.DEFAULT_REDIS, value = {
      CacheNames.FIND_DETAIL_BY_DS_ATTRIBUTE_NAME}, key = "#dsAttributeName")
  public void evictCacheByDsAttributeName(String dsAttributeName) {
  }

  @Override
  @Cacheable(cacheManager = RedisConstants.DEFAULT_REDIS, value = {
      CacheNames.FIND_DETAIL_BY_DS_ATTRIBUTE_NAME}, key = "#dsAttributeName", unless = "#result "
      + "== null")
  public DSExtractionEntity fetchDSExtractionsByName(String dsAttributeName) {
    return dsExtractionAttributesRepository.findByDsAttributeName(dsAttributeName);
  }
}
