package com.gdn.x.productcategorybase.service.impl;

import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.util.ArrayList;
import java.util.Arrays;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.productcategorybase.config.MandatoryParameterHelper;
import com.gdn.x.productcategorybase.dto.CategoryUpdateHistoryDTO;
import com.gdn.x.productcategorybase.dto.response.WholesaleMappingResponse;
import com.gdn.x.productcategorybase.entity.Category;
import com.gdn.x.productcategorybase.entity.WholesalePriceConfiguration;
import com.gdn.x.productcategorybase.repository.CategoryWholesaleConfigRepository;
import com.gdn.x.productcategorybase.service.CategoryService;
import com.gdn.x.productcategorybase.service.DomainEventPublisherService;

public class CategoryWholesaleConfigServiceTest {
  private static final String STORE_ID = "storeId";
  private static final String CATEGORY_CODE = "categoryCode";
  private static final String CATEGORY_CODE1 = "categoryCode1";
  private static final String CATEGORY_ID = "categoryId";
  private static final String CATEGORY_ID1 = "categoryId1";
  private static final Integer QUANTITY = 10;
  private static final Double PERCENTAGE = 10D;
  private static final Double PRICE = 100000D;
  private static final String CONFIGURATION_TYPE = "PERCENTAGE";
  private static final boolean WHOLESALE_CONFIG_ENABLE = true;
  private static final String ID = "id";
  private static final String WHOLESALECONFIG =
      "[{\"quantity\":10,\"minWholesaleDiscount\"" + ":[{\"price\":100000.0,\"percentage\":10.0}]}]";

  private WholesalePriceConfiguration wholesalePriceConfiguration;
  private Category category;
  private Category category1;

  @InjectMocks
  private CategoryWholesaleConfigServiceImpl categoryWholesaleConfigService;

  @Mock
  private CategoryService categoryService;

  @Mock
  private CategoryWholesaleConfigRepository categoryWholesaleConfigRepository;

  @Mock
  private ApplicationCacheServiceBean applicationCacheServiceBean;

  @Mock
  private DomainEventPublisherService domainEventPublisherService;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private MandatoryParameterHelper mandatoryParameterHelper;

  @BeforeEach
  public void initialize() {
    MockitoAnnotations.initMocks(this);
    category = new Category();
    category.setId(CATEGORY_ID);
    category.setCategoryCode(CATEGORY_CODE);
    category.setWholesalePriceConfigEnabled(WHOLESALE_CONFIG_ENABLE);

    category1 = new Category();
    category1.setId(CATEGORY_ID1);
    category1.setCategoryCode(CATEGORY_CODE1);
    category1.setWholesalePriceConfigEnabled(WHOLESALE_CONFIG_ENABLE);

    wholesalePriceConfiguration = new WholesalePriceConfiguration();
    wholesalePriceConfiguration.setConfigurationType(CONFIGURATION_TYPE);
    wholesalePriceConfiguration.setWholesaleConfigs(WHOLESALECONFIG);
  }

  @Test
  public void findByStoreIdAndCategoryIdTest() throws Exception {
    Mockito.when(categoryService.findByStoreIdAndCategoryCode(STORE_ID, CATEGORY_CODE)).thenReturn(category);
    Mockito.when(categoryService.findByStoreIdAndId(STORE_ID, CATEGORY_ID)).thenReturn(category);
    Mockito.when(categoryWholesaleConfigRepository.findByStoreIdAndCategoryId(STORE_ID, CATEGORY_ID))
        .thenReturn(wholesalePriceConfiguration);
    WholesaleMappingResponse response =
        this.categoryWholesaleConfigService.findByStoreIdAndCategoryId(STORE_ID, CATEGORY_ID, CATEGORY_CODE);
    Mockito.verify(categoryWholesaleConfigRepository).findByStoreIdAndCategoryId(STORE_ID, CATEGORY_ID);
  }

  @Test
  public void findByStoreIdAndCategoryCodeTest() throws Exception {
    Mockito.when(categoryService.findByStoreIdAndCategoryCode(STORE_ID, CATEGORY_CODE)).thenReturn(category);
    Mockito.when(categoryService.findByStoreIdAndId(STORE_ID, CATEGORY_ID)).thenReturn(category);
    Mockito.when(categoryWholesaleConfigRepository.findByStoreIdAndCategoryId(STORE_ID, CATEGORY_ID))
        .thenReturn(wholesalePriceConfiguration);
    WholesaleMappingResponse response =
        this.categoryWholesaleConfigService.findByStoreIdAndCategoryId(STORE_ID, null, CATEGORY_CODE);
    Mockito.verify(categoryWholesaleConfigRepository).findByStoreIdAndCategoryId(STORE_ID, CATEGORY_ID);
  }

  @Test
  public void findByStoreIdAndCategoryIdForPricePercentageTest() throws Exception {
    wholesalePriceConfiguration.setConfigurationType("PRICE_PERCENTAGE");
    Mockito.when(categoryService.findByStoreIdAndCategoryCode(STORE_ID, CATEGORY_CODE)).thenReturn(category);
    Mockito.when(categoryService.findByStoreIdAndId(STORE_ID, CATEGORY_ID)).thenReturn(category);
    Mockito.when(categoryWholesaleConfigRepository.findByStoreIdAndCategoryId(STORE_ID, CATEGORY_ID))
        .thenReturn(wholesalePriceConfiguration);
    WholesaleMappingResponse response =
        this.categoryWholesaleConfigService.findByStoreIdAndCategoryId(STORE_ID, CATEGORY_ID, CATEGORY_CODE);
    Mockito.verify(categoryWholesaleConfigRepository).findByStoreIdAndCategoryId(STORE_ID, CATEGORY_ID);
  }

  @Test
  public void updateCategoryWithWholesaleMappingsForC1LevelTest() throws JsonProcessingException {
    Mockito.when(categoryWholesaleConfigRepository.findByStoreIdAndCategoryId(STORE_ID, CATEGORY_ID))
        .thenReturn(wholesalePriceConfiguration);
    Mockito.doNothing().when(this.applicationCacheServiceBean)
        .evictCategoryDetailCache(STORE_ID, CATEGORY_ID);
    Mockito.when(objectMapper.readValue(eq(WHOLESALECONFIG), any(TypeReference.class)))
        .thenReturn(new ArrayList<>());
    this.categoryWholesaleConfigService
        .updateCategoryWholesaleMappings(STORE_ID, wholesalePriceConfiguration, category, true);
    Mockito.verify(objectMapper, times(2))
        .readValue(anyString(), any(TypeReference.class));
    Mockito.verify(categoryWholesaleConfigRepository).findByStoreIdAndCategoryId(STORE_ID, CATEGORY_ID);
    Mockito.verify(objectMapper, times(2)).writeValueAsString(any());
    Assertions.assertEquals(CONFIGURATION_TYPE, wholesalePriceConfiguration.getConfigurationType());
    Assertions.assertEquals(WHOLESALECONFIG, wholesalePriceConfiguration.getWholesaleConfigs());
  }

  @Test
  public void updateCategoryWithWholesaleMappings_nullTest() throws JsonProcessingException {
    Mockito.when(categoryWholesaleConfigRepository.findByStoreIdAndCategoryId(STORE_ID, CATEGORY_ID))
        .thenReturn(new WholesalePriceConfiguration());
    CategoryUpdateHistoryDTO expected = this.categoryWholesaleConfigService
        .updateCategoryWholesaleMappings(STORE_ID, wholesalePriceConfiguration, category, false);
    assertNull(expected);
  }

  @Test
  public void updateCategoryWithWholesaleMappingsForCnLevelTest() throws JsonProcessingException {
    Mockito.when(categoryWholesaleConfigRepository.findByStoreIdAndCategoryId(STORE_ID, CATEGORY_ID)).thenReturn(null);
    Mockito.doNothing().when(this.applicationCacheServiceBean)
        .evictCategoryDetailCache(STORE_ID, CATEGORY_ID);
    Mockito.when(objectMapper.readValue(eq(WHOLESALECONFIG), any(TypeReference.class)))
        .thenReturn(new ArrayList<>());
    this.categoryWholesaleConfigService
        .updateCategoryWholesaleMappings(STORE_ID, wholesalePriceConfiguration, category, false);
    Mockito.verify(categoryWholesaleConfigRepository).findByStoreIdAndCategoryId(STORE_ID, CATEGORY_ID);
    Mockito.verify(objectMapper).writeValueAsString(any());
    Mockito.verify(objectMapper)
        .readValue(anyString(), any(TypeReference.class));
    Assertions.assertEquals(CONFIGURATION_TYPE, wholesalePriceConfiguration.getConfigurationType());
    Assertions.assertEquals(WHOLESALECONFIG, wholesalePriceConfiguration.getWholesaleConfigs());
  }

  @Test
  public void updateWholesaleConfigForChildCategoriesTest() throws Exception {
    Mockito.when(categoryService.findAllChildForC1CategoryCodesTree(STORE_ID, CATEGORY_CODE))
        .thenReturn(Arrays.asList(category1));
    Mockito.doNothing().when(this.applicationCacheServiceBean)
        .evictCategoryDetailCache(STORE_ID, CATEGORY_ID);
    Mockito.when(objectMapper.readValue(eq(WHOLESALECONFIG), any(TypeReference.class)))
        .thenReturn(new ArrayList<>());
    this.categoryWholesaleConfigService
        .updateWholesaleConfigForChildCategories(STORE_ID, wholesalePriceConfiguration, category);
    Mockito.verify(categoryService).findAllChildForC1CategoryCodesTree(STORE_ID, CATEGORY_CODE);
    Mockito.verify(objectMapper).writeValueAsString(any());
    Mockito.verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    Assertions.assertEquals(CONFIGURATION_TYPE, wholesalePriceConfiguration.getConfigurationType());
    Assertions.assertEquals(WHOLESALECONFIG, wholesalePriceConfiguration.getWholesaleConfigs());
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(objectMapper);
    verifyNoMoreInteractions(domainEventPublisherService);
    verifyNoMoreInteractions(mandatoryParameterHelper);

  }
}
