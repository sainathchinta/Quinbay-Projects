package com.gdn.mta.product.service;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.lang3.tuple.Pair;
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
import com.gda.mta.product.dto.ProductBundleRecipeRequest;
import com.gda.mta.product.dto.L5HistoryDTO;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.product.entity.ProductBundleRecipe;
import com.gdn.mta.product.entity.ProductItemBusinessPartner;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.outbound.xProduct.XProductOutbound;
import com.gdn.x.product.model.vo.BundleRecipeVo;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.rest.web.model.response.SimpleMapStringResponse;

public class BundleRecipeServiceImplTest {

  private static final String STORE_ID = "storeId";
  private static final String ID1 = "id1";
  private static final String ID2 = "id2";
  private static final String ITEM_SKU1 = "itemSku1";
  private static final String ITEM_SKU2 = "itemSku2";
  private static final String ITEM_NAME1 = "itemName1";
  private static final String PP_CODE1 = "ppCode1";
  private static final String PP_CODE2 = "ppCode2";
  private static final String NOTE = "note";
  private static final int QUANTITY_1 = 2;
  private static final int QUANTITY_2 = 4;
  private static final String SKU_1 = "sku1";
  private static final String SKU_2 = "sku2";
  private static final String BUNDLE_RECIPE_HISTORY = "2*sku1 + 4*sku2";
  private static final String BUNDLE_RECIPE_HISTORY_1 = "2*sku1";

  private ProductItemBusinessPartner productItemBusinessPartner;
  private ProductItemBusinessPartner productItemBusinessPartner1;
  private ProductItemBusinessPartner productItemBusinessPartner2;
  private ProductBundleRecipeRequest bundleRecipeRequest1;
  private ProductBundleRecipeRequest bundleRecipeRequest2;
  private ProductBundleRecipe productBundleRecipe1;
  private ProductBundleRecipe productBundleRecipe2;
  private Map<String, String> itemSkuAndNameMap;
  private BundleRecipeVo bundleRecipeVo;

  @InjectMocks
  private BundleRecipeServiceImpl bundleRecipeService;

  @Mock
  private ProductItemBusinessPartnerService productItemBusinessPartnerService;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private XProductOutbound xProductOutbound;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    productItemBusinessPartner = new ProductItemBusinessPartner();
    productItemBusinessPartner.setId(ID1);
    productItemBusinessPartner1 = new ProductItemBusinessPartner();
    productItemBusinessPartner1.setId(ID1);
    productItemBusinessPartner1.setGdnProductItemSku(ITEM_SKU1);
    productItemBusinessPartner1.setPickupPointId(PP_CODE1);
    productItemBusinessPartner2 = new ProductItemBusinessPartner();
    productItemBusinessPartner2.setId(ID2);
    productItemBusinessPartner2.setGdnProductItemSku(ITEM_SKU2);
    productItemBusinessPartner2.setPickupPointId(PP_CODE2);
    bundleRecipeRequest1 = new ProductBundleRecipeRequest();
    bundleRecipeRequest1.setItemSku(ITEM_SKU1);
    bundleRecipeRequest1.setBundleRecipe(new HashSet<>());
    bundleRecipeRequest2 = new ProductBundleRecipeRequest();
    bundleRecipeRequest2.setItemSku(ITEM_SKU2);
    productBundleRecipe1 = new ProductBundleRecipe();
    productBundleRecipe1.setQuantity(QUANTITY_1);
    productBundleRecipe1.setItemSku(SKU_1);
    productBundleRecipe2 = new ProductBundleRecipe();
    productBundleRecipe2.setQuantity(QUANTITY_2);
    productBundleRecipe2.setItemSku(SKU_2);
    itemSkuAndNameMap = new HashMap<>();
    itemSkuAndNameMap.put(ITEM_SKU1, ITEM_NAME1);
    bundleRecipeVo = new BundleRecipeVo();
    bundleRecipeVo.setItemSku(ITEM_SKU1);
    bundleRecipeVo.setQuantity(QUANTITY_1);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(productItemBusinessPartnerService);
    Mockito.verifyNoMoreInteractions(objectMapper);
    Mockito.verifyNoMoreInteractions(xProductOutbound);
  }

  @Test
  public void convertToItemSkuAndBundlingRecipeMapTest() throws JsonProcessingException {
    Map<String, Set<ProductBundleRecipe>> itemSkuAndBundlingRecipeMap =
        bundleRecipeService.convertToItemSkuAndBundlingRecipeMap(Arrays.asList(bundleRecipeRequest1));
    Assertions.assertEquals(itemSkuAndBundlingRecipeMap.get(bundleRecipeRequest1.getItemSku()),
        bundleRecipeRequest1.getBundleRecipe());
  }

  @Test
  public void updateBundleRecipeInProductItemBusinessPartnerTest()
      throws JsonProcessingException, ApplicationException {
    Mockito.when(objectMapper.writeValueAsString(any())).thenReturn(NOTE);
    Mockito.when(xProductOutbound.getItemNameByItemSkus(any(SimpleListStringRequest.class), eq(true)))
        .thenReturn(new GdnRestSingleResponse(new SimpleMapStringResponse(), ID1));
    Mockito.when(productItemBusinessPartnerService.getProductItemBusinessPartnerByItemSkuList(STORE_ID,
        Arrays.asList(ITEM_SKU2, ITEM_SKU1))).thenReturn(Arrays.asList(productItemBusinessPartner1));
    Mockito.when(productItemBusinessPartnerService.replaceNewWithExistingProductItemBusinessPartner(
        Arrays.asList(productItemBusinessPartner), Arrays.asList(productItemBusinessPartner1))).thenReturn(
        Arrays.asList(productItemBusinessPartner1, productItemBusinessPartner2, productItemBusinessPartner));
    Pair<List<ProductItemBusinessPartner>, List<L5HistoryDTO>> productItemBusinessPartnerList =
        bundleRecipeService.updateBundleRecipeInProductItemBusinessPartner(STORE_ID,
            Arrays.asList(productItemBusinessPartner), Arrays.asList(bundleRecipeRequest1, bundleRecipeRequest2));
    Mockito.verify(productItemBusinessPartnerService).getProductItemBusinessPartnerByItemSkuList(STORE_ID,
        Arrays.asList(ITEM_SKU2, ITEM_SKU1));
    Mockito.verify(productItemBusinessPartnerService).replaceNewWithExistingProductItemBusinessPartner(
        Arrays.asList(productItemBusinessPartner), Arrays.asList(productItemBusinessPartner1));
    Mockito.verify(objectMapper, times(3)).writeValueAsString(any());
    Mockito.verify(xProductOutbound).getItemNameByItemSkus(any(SimpleListStringRequest.class), eq(true));
    Assertions.assertEquals(productItemBusinessPartner1.getBundleRecipe(), NOTE);
    Assertions.assertEquals(productItemBusinessPartner2.getBundleRecipe(), NOTE);
  }

  @Test
  public void updateBundleRecipeInProductItemBusinessPartnerEmptyBundlingTest()
      throws JsonProcessingException, ApplicationException {
    Pair<List<ProductItemBusinessPartner>, List<L5HistoryDTO>> productItemBusinessPartnerList =
        bundleRecipeService.updateBundleRecipeInProductItemBusinessPartner(STORE_ID,
            Arrays.asList(productItemBusinessPartner), Arrays.asList());
    Assertions.assertEquals(productItemBusinessPartnerList.getKey().size(), 0);
  }

  @Test
  public void bundleRecipeHistoryInNeedRevisionTest() {
    Map<String, Map<String, String>> bundleHistoryMap = new HashMap<>();
    bundleRecipeService.bundleRecipeHistoryInNeedRevision(ITEM_SKU1, bundleHistoryMap,
        new HashSet<>(Arrays.asList(productBundleRecipe1, productBundleRecipe2)),
        new HashSet<>(Arrays.asList(productBundleRecipe1)), itemSkuAndNameMap);
    Assertions.assertEquals(bundleHistoryMap.get(ITEM_SKU1).get(Constants.PREVIOUS_VALUE),
        BUNDLE_RECIPE_HISTORY_1);
    Assertions.assertEquals(bundleHistoryMap.get(ITEM_SKU1).get(Constants.CURRENT_VALUE),
        BUNDLE_RECIPE_HISTORY);
    Assertions.assertEquals(bundleHistoryMap.get(ITEM_SKU1).get(Constants.ITEM_NAME),
        ITEM_NAME1);
  }

  @Test
  public void bundleRecipeHistoryInNeedRevisionNoChangeTest() {
    Map<String, Map<String, String>> bundleHistoryMap = new HashMap<>();
    bundleRecipeService.bundleRecipeHistoryInNeedRevision(ITEM_SKU1, bundleHistoryMap,
        new HashSet<>(Arrays.asList(productBundleRecipe1)),
        new HashSet<>(Arrays.asList(productBundleRecipe1)), itemSkuAndNameMap);
    Assertions.assertEquals(bundleHistoryMap.size(), 0);
  }

  @Test
  public void convertProductBundleRecipeListToHistoryTest() {
    String history = bundleRecipeService.convertProductBundleRecipeListToHistory(new HashSet<>(
        Arrays.asList(productBundleRecipe1, productBundleRecipe2)));
    Assertions.assertEquals(history, BUNDLE_RECIPE_HISTORY);
  }

  @Test
  public void convertProductBundleRecipeListToEmptyHistoryTest() {
    String history = bundleRecipeService.convertProductBundleRecipeListToHistory(new HashSet<>());
    Assertions.assertEquals(history, Constants.HYPHEN);
  }

  @Test
  public void convertStringToBundleRecipeRequestListTest() throws JsonProcessingException {
    Mockito.when(objectMapper.readValue(Mockito.eq(NOTE), any(TypeReference.class)))
        .thenReturn(new HashSet<>(Arrays.asList(new ProductBundleRecipe())));
    Set<ProductBundleRecipe> bundleRecipe = bundleRecipeService
        .convertStringToBundleRecipeRequestList(NOTE);
    Mockito.verify(objectMapper).readValue(Mockito.eq(NOTE), any(TypeReference.class));
    Assertions.assertEquals(bundleRecipe, new HashSet<>(Arrays.asList(new ProductBundleRecipe())));
  }

  @Test
  public void convertStringToBundleRecipeRequestListEmptyTest() throws JsonProcessingException {
    Set<ProductBundleRecipe> bundleRecipe = bundleRecipeService.convertStringToBundleRecipeRequestList(null);
    Assertions.assertEquals(bundleRecipe, new HashSet<>());
  }

  @Test
  public void itemNameAndItemSkuMapTest() throws ApplicationException {
    Mockito.when(xProductOutbound.getItemNameByItemSkus(new SimpleListStringRequest(Arrays.asList(
        ITEM_SKU1), false), true)).thenReturn(new
        GdnRestSingleResponse(new SimpleMapStringResponse(), ID1));
    bundleRecipeService.itemNameAndItemSkuMap(Set.of(ITEM_SKU1));
    Mockito.verify(xProductOutbound).getItemNameByItemSkus(new SimpleListStringRequest(Arrays.asList(
        ITEM_SKU1), false), true);
  }

  @Test
  public void convertBundleRecipeToStringTest() throws JsonProcessingException {
    Mockito.when(objectMapper.writeValueAsString(any())).thenReturn(NOTE);
    String bundleRecipe = bundleRecipeService.convertBundleRecipeToString(new HashSet<>(Arrays.asList(bundleRecipeVo)));
    Mockito.verify(objectMapper).writeValueAsString(any());
    Assertions.assertEquals(bundleRecipe, NOTE);
  }

  @Test
  public void convertBundleRecipeToStringEmptyTest() throws JsonProcessingException {
    Mockito.when(objectMapper.writeValueAsString(any())).thenReturn(NOTE);
    String bundleRecipe = bundleRecipeService.convertBundleRecipeToString(new HashSet<>());
    Mockito.verify(objectMapper).writeValueAsString(any());
    Assertions.assertEquals(bundleRecipe, NOTE);

  }

}
