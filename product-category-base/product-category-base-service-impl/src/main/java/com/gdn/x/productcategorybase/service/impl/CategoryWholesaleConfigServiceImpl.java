package com.gdn.x.productcategorybase.service.impl;

import java.util.Comparator;
import java.util.LinkedList;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import com.gdn.common.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.config.MandatoryParameterHelper;
import com.gdn.x.productcategorybase.dto.CategoryUpdateHistoryDTO;
import com.gdn.x.productcategorybase.dto.WholesaleConfigDTO;
import com.gdn.x.productcategorybase.dto.WholesaleMappingDTO;
import com.gdn.x.productcategorybase.dto.response.MinWholesaleDiscountResponse;
import com.gdn.x.productcategorybase.dto.response.WholesaleConfigResponse;
import com.gdn.x.productcategorybase.dto.response.WholesaleMappingResponse;
import com.gdn.x.productcategorybase.entity.Category;
import com.gdn.x.productcategorybase.entity.WholesalePriceConfiguration;
import com.gdn.x.productcategorybase.repository.CategoryWholesaleConfigRepository;
import com.gdn.x.productcategorybase.service.CategoryService;
import com.gdn.x.productcategorybase.service.CategoryWholesaleConfigService;
import com.gdn.x.productcategorybase.service.DomainEventPublisherService;
import com.gdn.x.productcategorybase.util.ConverterUtil;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class CategoryWholesaleConfigServiceImpl implements CategoryWholesaleConfigService {

  @Autowired
  private CategoryWholesaleConfigRepository categoryWholesaleConfigRepository;

  @Autowired
  private CategoryService categoryService;

  @Autowired
  private ApplicationCacheServiceBean applicationCacheServiceBean;

  @Autowired
  private DomainEventPublisherService domainEventPublisherService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Override
  public WholesaleMappingResponse findByStoreIdAndCategoryId(String storeId, String categoryId, String categoryCode)
      throws Exception {
    Category category;
    WholesaleMappingResponse wholesaleMappingResponse = new WholesaleMappingResponse();
    ObjectMapper mapper = new ObjectMapper();
    log.debug("findByStoreIdAndCategoryId categoryId : {}, categoryCode : {}", categoryId, categoryCode);
    if (StringUtils.isBlank(categoryId) && StringUtils.isNotBlank(categoryCode)) {
      category = categoryService.findByStoreIdAndCategoryCode(storeId, categoryCode);
      categoryId = category.getId();
      wholesaleMappingResponse.setWholesalePriceConfigEnabled(category.isWholesalePriceConfigEnabled());
    } else {
      category = categoryService.findByStoreIdAndId(storeId, categoryId);
    }
    WholesalePriceConfiguration wholesalePriceConfiguration =
        categoryWholesaleConfigRepository.findByStoreIdAndCategoryId(storeId, categoryId);
    if (Objects.nonNull(wholesalePriceConfiguration)) {
      BeanUtils.copyProperties(wholesalePriceConfiguration, wholesaleMappingResponse);
      wholesaleMappingResponse.setConfigurationType(wholesalePriceConfiguration.getConfigurationType());
      List<WholesaleConfigResponse> wholesaleConfigResponseList = mapper
          .readValue(wholesalePriceConfiguration.getWholesaleConfigs(),
              new TypeReference<List<WholesaleConfigResponse>>() {
              });
      wholesaleMappingResponse.setWholesaleConfig(wholesaleConfigResponseList);
    }
    wholesaleMappingResponse.setWholesalePriceConfigEnabled(category.isWholesalePriceConfigEnabled());
    wholesaleMappingResponse = validationWholesaleConfig(wholesaleMappingResponse);
    log.debug("findByStoreIdAndCategoryId wholesaleMappingResponse: {}", wholesaleMappingResponse);
    return wholesaleMappingResponse;
  }

  public WholesaleMappingResponse validationWholesaleConfig(WholesaleMappingResponse wholesaleMappingResponse) {
    wholesaleMappingResponse.getWholesaleConfig().sort(Comparator.comparingInt(WholesaleConfigResponse::getQuantity));
    if (!StringUtils
        .equalsIgnoreCase(wholesaleMappingResponse.getConfigurationType(), Constants.WHOLESALE_CONFIG_TYPE_PERCENTAGE)) {
      for (WholesaleConfigResponse wholesaleConfigResponse : wholesaleMappingResponse.getWholesaleConfig()) {
        wholesaleConfigResponse.getMinWholesaleDiscount()
            .sort(Comparator.comparingDouble(MinWholesaleDiscountResponse::getPrice));
      }
    }
    return wholesaleMappingResponse;
  }

  @Override
  public CategoryUpdateHistoryDTO updateCategoryWholesaleMappings(String storeId,
      WholesalePriceConfiguration wholesalePriceConfiguration, Category category,
      boolean isParentCategory) throws JsonProcessingException {
    WholesalePriceConfiguration currentWholesalePriceConfiguration =
        categoryWholesaleConfigRepository.findByStoreIdAndCategoryId(storeId, category.getId());
    if (Objects.nonNull(currentWholesalePriceConfiguration) && isParentCategory) {
      WholesalePriceConfiguration previousWholesalePriceConfiguration =
          new WholesalePriceConfiguration();
      BeanUtils.copyProperties(currentWholesalePriceConfiguration,
          previousWholesalePriceConfiguration);
      WholesaleMappingDTO previousWholesaleMappingDTO = ConverterUtil.convertToWholesaleConfigDTO(
          previousWholesalePriceConfiguration.getConfigurationType(),
          objectMapper.readValue(previousWholesalePriceConfiguration.getWholesaleConfigs(),
              new TypeReference<List<WholesaleConfigDTO>>() {}));
      currentWholesalePriceConfiguration.setConfigurationType(wholesalePriceConfiguration.getConfigurationType());
      currentWholesalePriceConfiguration.setWholesaleConfigs(wholesalePriceConfiguration.getWholesaleConfigs());
      WholesaleMappingDTO currentWholesaleMappingDTO = ConverterUtil.convertToWholesaleConfigDTO(
          currentWholesalePriceConfiguration.getConfigurationType(),
          objectMapper.readValue(currentWholesalePriceConfiguration.getWholesaleConfigs(),
              new TypeReference<List<WholesaleConfigDTO>>() {}));
      log.debug("updating categoryId : {}, isParent : {}, wholesalePriceConfiguration {}", category.getId(),
          isParentCategory, wholesalePriceConfiguration);
      this.updateCategoryWholesaleConfiguration(storeId, currentWholesalePriceConfiguration);

      return CategoryUpdateHistoryDTO.builder()
          .currentValue(objectMapper.writeValueAsString(currentWholesaleMappingDTO))
          .previousValue(objectMapper.writeValueAsString(previousWholesaleMappingDTO))
          .category(category)
          .build();
    } else if (Objects.isNull(currentWholesalePriceConfiguration)) {
      WholesalePriceConfiguration newWholesalePriceConfiguration =
          getNewWholesalePriceConfiguration(wholesalePriceConfiguration, category);
      WholesaleMappingDTO newWholesaleMappingDTO = ConverterUtil.convertToWholesaleConfigDTO(
          newWholesalePriceConfiguration.getConfigurationType(),
          objectMapper.readValue(newWholesalePriceConfiguration.getWholesaleConfigs(),
              new TypeReference<List<WholesaleConfigDTO>>() {}));
      log.debug("Adding new wholesale config categoryId : {}, isParent : {}, wholesalePriceConfiguration {}",
          category.getId(), isParentCategory, newWholesalePriceConfiguration);
      this.updateCategoryWholesaleConfiguration(storeId, newWholesalePriceConfiguration);
      return CategoryUpdateHistoryDTO.builder()
          .currentValue(objectMapper.writeValueAsString(newWholesaleMappingDTO))
          .previousValue(null)
          .category(category)
          .build();
    }
    return null;
  }

  private WholesalePriceConfiguration getNewWholesalePriceConfiguration(
      WholesalePriceConfiguration wholesalePriceConfiguration, Category category) {
    WholesalePriceConfiguration newWholesalePriceConfiguration = new WholesalePriceConfiguration();
    BeanUtils.copyProperties(wholesalePriceConfiguration, newWholesalePriceConfiguration);
    newWholesalePriceConfiguration.setCategoryId(category.getId());
    return newWholesalePriceConfiguration;
  }

  @Override
  public void updateCategoryWholesaleConfiguration(String storeId,
      WholesalePriceConfiguration wholesalePriceConfiguration) {
    this.categoryWholesaleConfigRepository.save(wholesalePriceConfiguration);
    this.applicationCacheServiceBean
        .evictCategoryDetailCache(storeId, wholesalePriceConfiguration.getCategoryId());
  }

  @Override
  @Async
  public void updateWholesaleConfigForChildCategories(String storeId,
      WholesalePriceConfiguration wholesalePriceConfiguration, Category category) throws Exception {
    List<Category> allChildCategories =
        new LinkedList<>(this.categoryService.findAllChildForC1CategoryCodesTree(storeId, category.getCategoryCode()));
    allChildCategories.remove(category);
    if (CollectionUtils.isNotEmpty(allChildCategories)) {
      List<String> categoryIds = allChildCategories.stream().map(Category::getId).collect(Collectors.toList());
      log.debug("adding the wholesale configuration mapping {}  for all categoryId : {}", wholesalePriceConfiguration,
          categoryIds);
      for (Category childCategory : allChildCategories) {
        this.updateCategoryWholesaleMappings(storeId, wholesalePriceConfiguration, childCategory, false);
      }
    }
  }

}
