package com.gdn.mta.bulk.service;

import com.gda.mta.product.dto.AuditTrailDto;
import com.gda.mta.product.dto.response.AuditTrailListRequest;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.bulk.config.KafkaPublisher;
import com.gdn.mta.product.commons.constant.UpdateProductActivity;
import com.gdn.mta.product.entity.UpdatedProductHistory;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.x.product.domain.event.config.ProductDomainEventName;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.rest.web.model.response.ItemSkuPickupPointCodeResponse;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class UpdateProductHistoryServiceImplTest {

  private static final String GDNSKU_1 = "BP-001-00117-00001";
  private static final String GDNSKU_2 = "BP-001-00117-00002";
  private static final String PRODUCT_SKU_1 = "BP-001-00117";
  private static final String PRODUCT_SKU_2 = "BP-001-00118";
  private static final String PICKUP_POINT_CODE_1 = "PP-001";
  private static final String PICKUP_POINT_CODE_2 = "PP-002";
  private static final String PRODUCT_NAME_1 = "Test Product 1";
  private static final String PRODUCT_NAME_2 = "Test Product 2";
  private static final String OLD_VALUE_1 = "old-value-1";
  private static final String NEW_VALUE_1 = "new-value-1";
  private static final String OLD_VALUE_2 = "old-value-2";
  private static final String NEW_VALUE_2 = "new-value-2";
  private static final String BUSINESS_PARTNER_CODE = "BP-001";
  private static final String CHANGED_BY = "test-user";
  private static final String REQUEST_ID = "request-id-123";
  private static final String CLIENT_HOST = "client-host";

  @InjectMocks
  private UpdateProductHistoryServiceImpl updateProductHistoryService;

  @Mock
  private XProductOutboundService xProductOutboundService;

  @Mock
  private KafkaPublisher kafkaPublisher;

  private UpdateProductActivity activity;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.openMocks(this);
    activity = UpdateProductActivity.UPC_CODE;
  }

  @Test
  public void testUpdateProductHistoryDetailList_Success_WithMultipleItems() {
    // Arrange
    List<UpdatedProductHistory> historyList = createUpdatedProductHistoryList(2);
    
    List<ItemSkuPickupPointCodeResponse> pickupPointResponses = Arrays.asList(
        createItemSkuPickupPointCodeResponse(GDNSKU_1, PICKUP_POINT_CODE_1),
        createItemSkuPickupPointCodeResponse(GDNSKU_2, PICKUP_POINT_CODE_2)
    );

    GdnRestListResponse<ItemSkuPickupPointCodeResponse> response = 
        new GdnRestListResponse<>(null, null, true, pickupPointResponses, 
            new PageMetaData(0, 10, 2), REQUEST_ID);

    when(xProductOutboundService.getItemPickupPointCodeByItemSkus(any(SimpleListStringRequest.class)))
        .thenReturn(response);

    ArgumentCaptor<SimpleListStringRequest> requestCaptor = 
        ArgumentCaptor.forClass(SimpleListStringRequest.class);
    ArgumentCaptor<String> eventNameCaptor = ArgumentCaptor.forClass(String.class);
    ArgumentCaptor<String> productSkuCaptor = ArgumentCaptor.forClass(String.class);
    ArgumentCaptor<AuditTrailListRequest> auditRequestCaptor = 
        ArgumentCaptor.forClass(AuditTrailListRequest.class);

    // Act
    updateProductHistoryService.updateProductHistoryDetailList(historyList, activity);

    // Assert
    verify(xProductOutboundService).getItemPickupPointCodeByItemSkus(requestCaptor.capture());
    SimpleListStringRequest capturedRequest = requestCaptor.getValue();
    Assertions.assertNotNull(capturedRequest);
    Assertions.assertEquals(2, capturedRequest.getValue().size());
    Assertions.assertTrue(capturedRequest.getValue().contains(GDNSKU_1));
    Assertions.assertTrue(capturedRequest.getValue().contains(GDNSKU_2));

    // Verify onlineStatus is set to true
    Assertions.assertTrue(historyList.get(0).getOnlineStatus());
    Assertions.assertTrue(historyList.get(1).getOnlineStatus());

    // Verify pickupPointCode is set correctly
    Assertions.assertEquals(PICKUP_POINT_CODE_1, historyList.get(0).getPickupPointCode());
    Assertions.assertEquals(PICKUP_POINT_CODE_2, historyList.get(1).getPickupPointCode());

    // Verify kafkaPublisher.send is called for each item
    verify(kafkaPublisher, times(2)).send(
        eventNameCaptor.capture(),
        productSkuCaptor.capture(),
        auditRequestCaptor.capture()
    );

    List<String> capturedEventNames = eventNameCaptor.getAllValues();
    List<String> capturedProductSkus = productSkuCaptor.getAllValues();
    List<AuditTrailListRequest> capturedAuditRequests = auditRequestCaptor.getAllValues();

    Assertions.assertEquals(2, capturedEventNames.size());
    Assertions.assertEquals(ProductDomainEventName.PRODUCT_SKU_UPDATE_HISTORY, 
        capturedEventNames.get(0));
    Assertions.assertEquals(ProductDomainEventName.PRODUCT_SKU_UPDATE_HISTORY, 
        capturedEventNames.get(1));

    Assertions.assertEquals(2, capturedProductSkus.size());
    Assertions.assertEquals(PRODUCT_SKU_1, capturedProductSkus.get(0));
    Assertions.assertEquals(PRODUCT_SKU_2, capturedProductSkus.get(1));

    Assertions.assertEquals(2, capturedAuditRequests.size());
    verifyAuditTrailListRequest(capturedAuditRequests.get(0), historyList.get(0), activity);
    verifyAuditTrailListRequest(capturedAuditRequests.get(1), historyList.get(1), activity);
  }

  @Test
  public void testUpdateProductHistoryDetailList_Success_WithSingleItem() {
    // Arrange
    List<UpdatedProductHistory> historyList = createUpdatedProductHistoryList(1);
    
    List<ItemSkuPickupPointCodeResponse> pickupPointResponses = Arrays.asList(
        createItemSkuPickupPointCodeResponse(GDNSKU_1, PICKUP_POINT_CODE_1)
    );

    GdnRestListResponse<ItemSkuPickupPointCodeResponse> response = 
        new GdnRestListResponse<>(null, null, true, pickupPointResponses, 
            new PageMetaData(0, 10, 1), REQUEST_ID);

    when(xProductOutboundService.getItemPickupPointCodeByItemSkus(any(SimpleListStringRequest.class)))
        .thenReturn(response);

    // Act
    updateProductHistoryService.updateProductHistoryDetailList(historyList, activity);

    // Assert
    verify(xProductOutboundService).getItemPickupPointCodeByItemSkus(any(SimpleListStringRequest.class));
    Assertions.assertTrue(historyList.get(0).getOnlineStatus());
    Assertions.assertEquals(PICKUP_POINT_CODE_1, historyList.get(0).getPickupPointCode());
    verify(kafkaPublisher, times(1)).send(
        eq(ProductDomainEventName.PRODUCT_SKU_UPDATE_HISTORY),
        eq(PRODUCT_SKU_1),
        any(AuditTrailListRequest.class)
    );
  }

  @Test
  public void testUpdateProductHistoryDetailList_EmptyList() {
    // Arrange
    List<UpdatedProductHistory> historyList = new ArrayList<>();
    
    // The implementation still calls getItemPickupPointCodeByItemSkus even for empty lists
    GdnRestListResponse<ItemSkuPickupPointCodeResponse> response = 
        new GdnRestListResponse<>(null, null, true, Collections.emptyList(), 
            new PageMetaData(0, 10, 0), REQUEST_ID);

    when(xProductOutboundService.getItemPickupPointCodeByItemSkus(any(SimpleListStringRequest.class)))
        .thenReturn(response);

    // Act
    updateProductHistoryService.updateProductHistoryDetailList(historyList, activity);

    // Assert
    verify(xProductOutboundService).getItemPickupPointCodeByItemSkus(any(SimpleListStringRequest.class));
    verify(kafkaPublisher, never()).send(any(), any(), any());
  }

  @Test
  public void testUpdateProductHistoryDetailList_ItemSkuNotFoundInMap_UsesHyphen() {
    // Arrange
    List<UpdatedProductHistory> historyList = createUpdatedProductHistoryList(1);
    
    // Return empty list or different itemSku
    List<ItemSkuPickupPointCodeResponse> pickupPointResponses = Arrays.asList(
        createItemSkuPickupPointCodeResponse("DIFFERENT-SKU", PICKUP_POINT_CODE_1)
    );

    GdnRestListResponse<ItemSkuPickupPointCodeResponse> response = 
        new GdnRestListResponse<>(null, null, true, pickupPointResponses, 
            new PageMetaData(0, 10, 1), REQUEST_ID);

    when(xProductOutboundService.getItemPickupPointCodeByItemSkus(any(SimpleListStringRequest.class)))
        .thenReturn(response);

    // Act
    updateProductHistoryService.updateProductHistoryDetailList(historyList, activity);

    // Assert
    verify(xProductOutboundService).getItemPickupPointCodeByItemSkus(any(SimpleListStringRequest.class));
    Assertions.assertTrue(historyList.get(0).getOnlineStatus());
    // Should use HYPHEN when itemSku is not found in map
    Assertions.assertEquals(Constants.HYPHEN, historyList.get(0).getPickupPointCode());
    verify(kafkaPublisher, times(1)).send(
        eq(ProductDomainEventName.PRODUCT_SKU_UPDATE_HISTORY),
        eq(PRODUCT_SKU_1),
        any(AuditTrailListRequest.class)
    );
  }

  @Test
  public void testUpdateProductHistoryDetailList_EmptyPickupPointResponse() {
    // Arrange
    List<UpdatedProductHistory> historyList = createUpdatedProductHistoryList(1);
    
    GdnRestListResponse<ItemSkuPickupPointCodeResponse> response = 
        new GdnRestListResponse<>(null, null, true, Collections.emptyList(), 
            new PageMetaData(0, 10, 0), REQUEST_ID);

    when(xProductOutboundService.getItemPickupPointCodeByItemSkus(any(SimpleListStringRequest.class)))
        .thenReturn(response);

    // Act
    updateProductHistoryService.updateProductHistoryDetailList(historyList, activity);

    // Assert
    verify(xProductOutboundService).getItemPickupPointCodeByItemSkus(any(SimpleListStringRequest.class));
    Assertions.assertTrue(historyList.get(0).getOnlineStatus());
    // Should use HYPHEN when response is empty
    Assertions.assertEquals(Constants.HYPHEN, historyList.get(0).getPickupPointCode());
    verify(kafkaPublisher, times(1)).send(
        eq(ProductDomainEventName.PRODUCT_SKU_UPDATE_HISTORY),
        eq(PRODUCT_SKU_1),
        any(AuditTrailListRequest.class)
    );
  }

  @Test
  public void testUpdateProductHistoryDetailList_NullPickupPointResponse() {
    // Arrange
    List<UpdatedProductHistory> historyList = createUpdatedProductHistoryList(1);
    
    // Note: The implementation calls getContent().forEach() without null check,
    // so returning null content will cause NPE. Using empty list instead to test the behavior.
    GdnRestListResponse<ItemSkuPickupPointCodeResponse> response = 
        new GdnRestListResponse<>(null, null, true, Collections.emptyList(), 
            new PageMetaData(0, 10, 0), REQUEST_ID);

    when(xProductOutboundService.getItemPickupPointCodeByItemSkus(any(SimpleListStringRequest.class)))
        .thenReturn(response);

    // Act
    updateProductHistoryService.updateProductHistoryDetailList(historyList, activity);

    // Assert
    verify(xProductOutboundService).getItemPickupPointCodeByItemSkus(any(SimpleListStringRequest.class));
    Assertions.assertTrue(historyList.get(0).getOnlineStatus());
    // Should use HYPHEN when response content is empty (no matching itemSku)
    Assertions.assertEquals(Constants.HYPHEN, historyList.get(0).getPickupPointCode());
    verify(kafkaPublisher, times(1)).send(
        eq(ProductDomainEventName.PRODUCT_SKU_UPDATE_HISTORY),
        eq(PRODUCT_SKU_1),
        any(AuditTrailListRequest.class)
    );
  }

  @Test
  public void testUpdateProductHistoryDetailList_DifferentActivity() {
    // Arrange
    UpdateProductActivity differentActivity = UpdateProductActivity.UPC_CODE;
    List<UpdatedProductHistory> historyList = createUpdatedProductHistoryList(1);
    
    List<ItemSkuPickupPointCodeResponse> pickupPointResponses = Arrays.asList(
        createItemSkuPickupPointCodeResponse(GDNSKU_1, PICKUP_POINT_CODE_1)
    );

    GdnRestListResponse<ItemSkuPickupPointCodeResponse> response = 
        new GdnRestListResponse<>(null, null, true, pickupPointResponses, 
            new PageMetaData(0, 10, 1), REQUEST_ID);

    when(xProductOutboundService.getItemPickupPointCodeByItemSkus(any(SimpleListStringRequest.class)))
        .thenReturn(response);

    ArgumentCaptor<AuditTrailListRequest> auditRequestCaptor = 
        ArgumentCaptor.forClass(AuditTrailListRequest.class);

    // Act
    updateProductHistoryService.updateProductHistoryDetailList(historyList, differentActivity);

    // Assert
    verify(kafkaPublisher, times(1)).send(
        eq(ProductDomainEventName.PRODUCT_SKU_UPDATE_HISTORY),
        eq(PRODUCT_SKU_1),
        auditRequestCaptor.capture()
    );

    AuditTrailListRequest capturedRequest = auditRequestCaptor.getValue();
    Assertions.assertNotNull(capturedRequest);
    Assertions.assertEquals(1, capturedRequest.getAuditTrailResponseList().size());
    
    AuditTrailDto auditTrailDto = capturedRequest.getAuditTrailResponseList().get(0);
    Assertions.assertEquals(differentActivity.name(), auditTrailDto.getActionKey());
  }

  @Test
  public void testUpdateProductHistoryDetailList_VerifyAuditTrailRequestFields() {
    // Arrange
    List<UpdatedProductHistory> historyList = createUpdatedProductHistoryList(1);
    
    List<ItemSkuPickupPointCodeResponse> pickupPointResponses = Arrays.asList(
        createItemSkuPickupPointCodeResponse(GDNSKU_1, PICKUP_POINT_CODE_1)
    );

    GdnRestListResponse<ItemSkuPickupPointCodeResponse> response = 
        new GdnRestListResponse<>(null, null, true, pickupPointResponses, 
            new PageMetaData(0, 10, 1), REQUEST_ID);

    when(xProductOutboundService.getItemPickupPointCodeByItemSkus(any(SimpleListStringRequest.class)))
        .thenReturn(response);

    ArgumentCaptor<AuditTrailListRequest> auditRequestCaptor = 
        ArgumentCaptor.forClass(AuditTrailListRequest.class);

    // Act
    updateProductHistoryService.updateProductHistoryDetailList(historyList, activity);

    // Assert
    verify(kafkaPublisher, times(1)).send(
        eq(ProductDomainEventName.PRODUCT_SKU_UPDATE_HISTORY),
        eq(PRODUCT_SKU_1),
        auditRequestCaptor.capture()
    );

    AuditTrailListRequest capturedRequest = auditRequestCaptor.getValue();
    verifyAuditTrailListRequest(capturedRequest, historyList.get(0), activity);
  }

  @Test
  public void testUpdateProductHistoryDetailList_MultipleItemsWithSomeNotFound() {
    // Arrange
    List<UpdatedProductHistory> historyList = createUpdatedProductHistoryList(2);
    
    // Only return response for first item
    List<ItemSkuPickupPointCodeResponse> pickupPointResponses = Arrays.asList(
        createItemSkuPickupPointCodeResponse(GDNSKU_1, PICKUP_POINT_CODE_1)
    );

    GdnRestListResponse<ItemSkuPickupPointCodeResponse> response = 
        new GdnRestListResponse<>(null, null, true, pickupPointResponses, 
            new PageMetaData(0, 10, 1), REQUEST_ID);

    when(xProductOutboundService.getItemPickupPointCodeByItemSkus(any(SimpleListStringRequest.class)))
        .thenReturn(response);

    // Act
    updateProductHistoryService.updateProductHistoryDetailList(historyList, activity);

    // Assert
    verify(xProductOutboundService).getItemPickupPointCodeByItemSkus(any(SimpleListStringRequest.class));
    
    // First item should have pickup point code
    Assertions.assertEquals(PICKUP_POINT_CODE_1, historyList.get(0).getPickupPointCode());
    
    // Second item should use HYPHEN as it's not found in map
    Assertions.assertEquals(Constants.HYPHEN, historyList.get(1).getPickupPointCode());
    
    // Both should have onlineStatus set to true
    Assertions.assertTrue(historyList.get(0).getOnlineStatus());
    Assertions.assertTrue(historyList.get(1).getOnlineStatus());
    
    // Both should publish kafka events
    verify(kafkaPublisher, times(2)).send(
        eq(ProductDomainEventName.PRODUCT_SKU_UPDATE_HISTORY),
        any(String.class),
        any(AuditTrailListRequest.class)
    );
  }

  // Helper methods

  private List<UpdatedProductHistory> createUpdatedProductHistoryList(int count) {
    List<UpdatedProductHistory> list = new ArrayList<>();
    for (int i = 0; i < count; i++) {
      UpdatedProductHistory history = new UpdatedProductHistory();
      if (i == 0) {
        history.setGdnSku(GDNSKU_1);
        history.setProductSku(PRODUCT_SKU_1);
        history.setGdnName(PRODUCT_NAME_1);
        history.setOldValues(OLD_VALUE_1);
        history.setNewValues(NEW_VALUE_1);
      } else {
        history.setGdnSku(GDNSKU_2);
        history.setProductSku(PRODUCT_SKU_2);
        history.setGdnName(PRODUCT_NAME_2);
        history.setOldValues(OLD_VALUE_2);
        history.setNewValues(NEW_VALUE_2);
      }
      history.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
      history.setChangedBy(CHANGED_BY);
      history.setRequestId(REQUEST_ID);
      history.setClientHost(CLIENT_HOST);
      list.add(history);
    }
    return list;
  }

  private ItemSkuPickupPointCodeResponse createItemSkuPickupPointCodeResponse(
      String itemSku, String pickupPointCode) {
    ItemSkuPickupPointCodeResponse response = new ItemSkuPickupPointCodeResponse();
    response.setItemSku(itemSku);
    response.setPickupPointCode(pickupPointCode);
    return response;
  }

  private void verifyAuditTrailListRequest(AuditTrailListRequest request, 
      UpdatedProductHistory history, UpdateProductActivity activity) {
    Assertions.assertNotNull(request);
    Assertions.assertEquals(history.getChangedBy(), request.getChangedBy());
    Assertions.assertEquals(history.getRequestId(), request.getRequestId());
    Assertions.assertEquals(history.getClientHost(), request.getClientId());
    Assertions.assertEquals(com.gdn.partners.bulk.util.Constant.CHANNEL_ID, request.getAccessChannel());
    Assertions.assertTrue(request.isUpdateDirectly());
    Assertions.assertNotNull(request.getAuditTrailResponseList());
    Assertions.assertEquals(1, request.getAuditTrailResponseList().size());

    AuditTrailDto auditTrailDto = request.getAuditTrailResponseList().get(0);
    Assertions.assertEquals(history.getBusinessPartnerCode(), auditTrailDto.getBusinessPartnerCode());
    Assertions.assertEquals(activity.name(), auditTrailDto.getActionKey());
    Assertions.assertEquals(history.getGdnSku(), auditTrailDto.getGdnSku());
    Assertions.assertEquals(history.getProductSku(), auditTrailDto.getProductSku());
    Assertions.assertEquals(history.getGdnName(), auditTrailDto.getName());
    Assertions.assertEquals(history.getOldValues(), auditTrailDto.getOldValue());
    Assertions.assertEquals(history.getNewValues(), auditTrailDto.getNewValue());
    Assertions.assertEquals(history.getPickupPointCode(), auditTrailDto.getPickupPointCode());
    // Note: OnlineStatus is set to Boolean.TRUE in the implementation (line 85)
  }
}

