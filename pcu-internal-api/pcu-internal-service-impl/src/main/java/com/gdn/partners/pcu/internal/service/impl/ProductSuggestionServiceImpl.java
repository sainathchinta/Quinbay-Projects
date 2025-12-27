package com.gdn.partners.pcu.internal.service.impl;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

import jakarta.annotation.PostConstruct;
import org.apache.commons.collections.MapUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Pageable;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Service;

import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.ext.catalog.rest.web.model.response.PristineCategoryMapResponse;
import com.gdn.partners.pcu.internal.client.feign.ExtCatalogFeign;
import com.gdn.partners.pcu.internal.properties.ExtCatalogProperties;
import com.gdn.partners.pcu.internal.service.ProductSuggestionService;
import com.gdn.partners.pcu.internal.service.impl.helper.ResponseHelper;
import com.gdn.partners.pcu.internal.web.model.request.CategoryWeb;
import com.gdn.x.productcategorybase.dto.response.ProductCodeResponse;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class ProductSuggestionServiceImpl implements ProductSuggestionService {

  private static final String REDIS_CATEGORY_KEY = "SUPPORTED_CATEGORIES_BY_PRISTINE";
  private static final String COMMA_DELIMITER = ",";

  @Autowired
  private ExtCatalogFeign extCatalogFeign;

  @Autowired
  private RedisTemplate<String, Map<String, Set<String>>> redisTemplate;

  @Autowired
  private ExtCatalogProperties extCatalogProperties;

  private static List<String> pristineCategoryList;

  @PostConstruct
  public void init() {
    pristineCategoryList = Arrays.asList(extCatalogProperties.getPristineCategories().split(COMMA_DELIMITER));
  }

  @Override
  public Map<String, Set<String>> getSupportedBlibliCategoriesByPristine() {
    Map<String, Set<String>> mapResponse = null;
    try {
      Map<String, Set<String>> supportedCategories =
          redisTemplate.boundValueOps(REDIS_CATEGORY_KEY).get();
      if (MapUtils.isEmpty(supportedCategories)) {
        GdnRestSingleResponse<PristineCategoryMapResponse> response =
            extCatalogFeign.getSupportedBlibliCategoriesByPristine();
        ResponseHelper.validateResponse(response);
        if (MapUtils.isNotEmpty(response.getValue().getCategoryMap())) {
          mapResponse = response.getValue().getCategoryMap();
          redisTemplate.boundValueOps(REDIS_CATEGORY_KEY).set(mapResponse);
          redisTemplate.expire(REDIS_CATEGORY_KEY, extCatalogProperties.getPristineTimeoutPeriod(), TimeUnit.HOURS);
        }
      } else {
        mapResponse = supportedCategories;
      }
      mapResponse = Optional.ofNullable(mapResponse).orElse(new HashMap<>()).entrySet()
          .stream()
          .filter(category -> pristineCategoryList.contains(category.getKey()))
          .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
    } catch (Exception e) {
      log.error("error getting product categories from ext-catalog", e);
    }
    return mapResponse;
  }

  @Override
  public List<ProductCodeResponse> getPCBProductCodes(String productCode, String category, Pageable pageable) {
    GdnRestListResponse<ProductCodeResponse> response = extCatalogFeign
        .getPCBProductCodes(productCode, CategoryWeb.valueOf(category).name(), pageable.getPageNumber(),
            pageable.getPageSize());
    ResponseHelper.validateResponse(response);
    return response.getContent();
  }
}
