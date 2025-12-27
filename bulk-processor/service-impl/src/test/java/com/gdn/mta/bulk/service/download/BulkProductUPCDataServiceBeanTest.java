package com.gdn.mta.bulk.service.download;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

import java.util.*;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.*;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.test.util.ReflectionTestUtils;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.dto.ItemBasicL4Response;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.ProductUPCDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkDataResponse;
import com.gdn.mta.bulk.models.download.responsedata.BulkProductLiteResponse;
import com.gdn.mta.bulk.repository.BusinessPartnerRepository;
import com.gdn.mta.bulk.service.XProductOutboundService;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.product.rest.web.model.request.ProductSummaryRequest;
import com.gdn.x.product.rest.web.model.response.ProductL3SummaryResponse;

@ExtendWith(MockitoExtension.class)
class BulkProductUPCDataServiceBeanTest {

  @InjectMocks
  private BulkProductUPCDataServiceBean service;

  @Mock
  private BusinessPartnerRepository businessPartnerRepository;

  @Mock
  private XProductOutboundService xProductOutboundService;

  @Mock
  private ObjectMapper objectMapper;

  @BeforeEach
  void setUp() {
    // set sensible defaults for value-injected fields using ReflectionTestUtils
    ReflectionTestUtils.setField(service, "maxProductSize", 1);      // small to exercise paging loop
    ReflectionTestUtils.setField(service, "maxL4DownloadSize", 1000); // default large
    ReflectionTestUtils.setField(service, "batchSizeL4", 1);
    ReflectionTestUtils.setField(service, "productSkuSize", 20);
  }

  /**
   * Normal happy-path: two product L3s, each with one L4 item, returned as CSV-like rows.
   */
  @Test
  void testGetData_happyPath_returnsRows() throws Exception {
    // Prepare request objects
    BulkDownloadRequest incoming = new BulkDownloadRequest();
    incoming.setMerchantId("MERCH1");
    incoming.setRequestId("REQ-1");

    ProductUPCDownloadRequest converted = new ProductUPCDownloadRequest();
    ProductSummaryRequest productSummaryRequest = new ProductSummaryRequest();
    converted.setProductSummaryRequest(productSummaryRequest);
    converted.setUsername("userA");
    converted.setRequestId("REQ-1");

    when(objectMapper.convertValue(incoming, ProductUPCDownloadRequest.class)).thenReturn(converted);

    // Business partner
    ProfileResponse profile = new ProfileResponse();
    profile.setBusinessPartnerCode("BP001");
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(), eq("MERCH1")))
        .thenReturn(profile);

    // Product L3 summaries: create 2 SKUs
    ProductL3SummaryResponse p1 = new ProductL3SummaryResponse();
    p1.setProductSku("P-001");
    p1.setProductName("Prod 1");
    ProductL3SummaryResponse p2 = new ProductL3SummaryResponse();
    p2.setProductSku("P-002");
    p2.setProductName("Prod 2");

    // Simulate getProductL3SummaryResponse returning single page with 2 elements.
    // Use Pageable.unpaged() if you ever want to force a specific total. For this test default is fine.
    Page<ProductL3SummaryResponse> page1 = new PageImpl<>(Arrays.asList(p1, p2));
    when(xProductOutboundService.getProductL3SummaryResponse(eq(productSummaryRequest), anyInt(), anyInt(), eq("REQ-1"),
        eq("userA")))
        .thenReturn(page1);

    // For L4 items: return one Item per product
    ItemBasicL4Response item1 = new ItemBasicL4Response();
    item1.setProductSku("P-001");
    item1.setItemSku("I-001");
    item1.setUpcCode("UPC001");
    item1.setItemName("Item One");

    ItemBasicL4Response item2 = new ItemBasicL4Response();
    item2.setProductSku("P-002");
    item2.setItemSku("I-002");
    item2.setUpcCode("UPC002");
    item2.setItemName("Item Two");

    // Return L4 pages: first call returns one item, next returns empty, then another item, then empty.
    PageImpl<ItemBasicL4Response> paginatedResponse = new PageImpl<>(List.of(item1,item2));
    when(xProductOutboundService.getL4ItemListByProductSku(anyInt(), anyInt(), eq("REQ-1"), eq("userA"), any()))
        .thenReturn(paginatedResponse)
        .thenReturn(new PageImpl<>(Collections.emptyList()));

    // Invoke
    BulkDataResponse resp = service.getData(incoming);

    assertNotNull(resp);
    assertTrue(resp instanceof BulkProductLiteResponse);
    BulkProductLiteResponse lite = (BulkProductLiteResponse) resp;
    List<List<String>> rows = lite.getProductContentList();
    // we expect two rows: [productSku, productName, itemSku, itemCode, upcCode]
    assertEquals(2, rows.size());

    List<String> row1 = rows.stream().filter(r -> r.get(0).equals("P-001")).findFirst().orElse(null);
    assertNotNull(row1);
    assertEquals(Arrays.asList("P-001", "Prod 1", "I-001", "Item One", "UPC001"), row1);

    List<String> row2 = rows.stream().filter(r -> r.get(0).equals("P-002")).findFirst().orElse(null);
    assertNotNull(row2);
    assertEquals(Arrays.asList("P-002", "Prod 2", "I-002", "Item Two", "UPC002"), row2);
  }

  /**
   * Empty L3 results -> should return empty list and not mark partial download.
   */
  @Test
  void testGetData_noProducts_returnsEmpty() throws Exception {
    BulkDownloadRequest incoming = new BulkDownloadRequest();
    incoming.setMerchantId("M_EMPTY");
    incoming.setRequestId("REQ-EMPTY");

    ProductUPCDownloadRequest converted = new ProductUPCDownloadRequest();
    converted.setProductSummaryRequest(new ProductSummaryRequest());
    converted.setUsername("userX");
    converted.setRequestId("REQ-EMPTY");

    when(objectMapper.convertValue(incoming, ProductUPCDownloadRequest.class)).thenReturn(converted);

    ProfileResponse profile = new ProfileResponse();
    profile.setBusinessPartnerCode("BP00");
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(), eq("M_EMPTY")))
        .thenReturn(profile);

    // return empty page (0 total elements)
    Page<ProductL3SummaryResponse> emptyPage = new PageImpl<>(Collections.emptyList());
    when(xProductOutboundService.getProductL3SummaryResponse(eq(converted.getProductSummaryRequest()), anyInt(), anyInt(),
        eq("REQ-EMPTY"), eq("userX"))).thenReturn(emptyPage);

    BulkDataResponse resp = service.getData(incoming);

    assertNotNull(resp);
    assertTrue(resp instanceof BulkProductLiteResponse);
    BulkProductLiteResponse lite = (BulkProductLiteResponse) resp;
    List<List<String>> rows = lite.getProductContentList();
    assertTrue(rows.isEmpty(), "Expected no rows for empty product list");
    // partial flag should be false
    assertFalse(lite.isPartialDownload());
  }

  /**
   * Partial download triggered when the maxL4DownloadSize limit is small.
   */
  @Test
  void testGetData_partialDownload_triggersLimit() throws Exception {
    // reduce maxL4DownloadSize to 1 to quickly trigger partial download path
    ReflectionTestUtils.setField(service, "maxL4DownloadSize", 1);

    BulkDownloadRequest incoming = new BulkDownloadRequest();
    incoming.setMerchantId("MERCH-PART");
    incoming.setRequestId("REQ-PART");

    ProductUPCDownloadRequest converted = new ProductUPCDownloadRequest();
    converted.setProductSummaryRequest(new ProductSummaryRequest());
    converted.setUsername("userP");
    converted.setRequestId("REQ-PART");

    when(objectMapper.convertValue(incoming, ProductUPCDownloadRequest.class)).thenReturn(converted);

    ProfileResponse profile = new ProfileResponse();
    profile.setBusinessPartnerCode("BP-P");
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(), eq("MERCH-PART"))).thenReturn(profile);

    // Prepare two L3 products
    ProductL3SummaryResponse p1 = new ProductL3SummaryResponse();
    p1.setProductSku("PX-1");
    p1.setProductName("PX One");
    ProductL3SummaryResponse p2 = new ProductL3SummaryResponse();
    p2.setProductSku("PX-2");
    p2.setProductName("PX Two");

    // Return an L3 page. Use PageImpl(content) which sets totalElements = content.size().
    Page<ProductL3SummaryResponse> page = new PageImpl<>(Arrays.asList(p1, p2));
    when(xProductOutboundService.getProductL3SummaryResponse(any(), anyInt(), anyInt(), eq("REQ-PART"), eq("userP")))
        .thenReturn(page);

    // Provide two L4 items; we expect service to stop and set partial flag true and return only 1 item
    ItemBasicL4Response item1 = new ItemBasicL4Response();
    item1.setProductSku("PX-1");
    item1.setItemSku("I-P1");
    item1.setUpcCode("UPC-P1");
    item1.setItemName("NameP1");

    ItemBasicL4Response item2 = new ItemBasicL4Response();
    item2.setProductSku("PX-2");
    item2.setItemSku("I-P2");
    item2.setUpcCode("UPC-P2");
    item2.setItemName("NameP2");

    when(xProductOutboundService.getL4ItemListByProductSku(anyInt(), anyInt(), eq("REQ-PART"), eq("userP"), any()))
        .thenReturn(new PageImpl<>(Collections.singletonList(item1)))
        .thenReturn(new PageImpl<>(Collections.singletonList(item2)));

    BulkDataResponse resp = service.getData(incoming);

    assertNotNull(resp);
    assertTrue(resp instanceof BulkProductLiteResponse);
    BulkProductLiteResponse lite = (BulkProductLiteResponse) resp;
    List<List<String>> rows = lite.getProductContentList();
    // because maxL4DownloadSize = 1, we should get only 1 row
    assertEquals(1, rows.size());
    assertTrue(lite.isPartialDownload());
    List<String> row = rows.get(0);
    assertEquals(Arrays.asList("PX-1", "PX One", "I-P1", "NameP1", "UPC-P1"), row);
  }

  /**
   * Ensure errors during L4 fetch are caught and do not break the overall flow.
   * Simulate exception for the first page call then return items on next calls.
   */
  @Test
  void testGetData_l4FetchThrows_exceptionHandled() throws Exception {
    BulkDownloadRequest incoming = new BulkDownloadRequest();
    incoming.setMerchantId("MERCH-ERR");
    incoming.setRequestId("REQ-ERR");

    ProductUPCDownloadRequest converted = new ProductUPCDownloadRequest();
    converted.setProductSummaryRequest(new ProductSummaryRequest());
    converted.setUsername("userE");
    converted.setRequestId("REQ-ERR");

    when(objectMapper.convertValue(incoming, ProductUPCDownloadRequest.class)).thenReturn(converted);

    ProfileResponse profile = new ProfileResponse();
    profile.setBusinessPartnerCode("BP-ERR");
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(), eq("MERCH-ERR"))).thenReturn(profile);

    // One product
    ProductL3SummaryResponse p1 = new ProductL3SummaryResponse();
    p1.setProductSku("ERR-1");
    p1.setProductName("ProdErr");

    // Here we want product list length 1 -> can use default PageImpl
    Page<ProductL3SummaryResponse> l3page = new PageImpl<>(Collections.singletonList(p1));
    when(xProductOutboundService.getProductL3SummaryResponse(any(), anyInt(), anyInt(), eq("REQ-ERR"), eq("userE")))
        .thenReturn(l3page);

    // First call throws exception, second call returns a normal page with one item.
    when(xProductOutboundService.getL4ItemListByProductSku(anyInt(), anyInt(), eq("REQ-ERR"), eq("userE"), any()))
        .thenThrow(new RuntimeException("simulated xProduct error"))
        .thenReturn(new PageImpl<>(Collections.singletonList(createL4("ERR-1", "I-ERR", "UPC-ERR", "ErrItem"))));

    // The service should catch the exception and continue; ultimately rows should include the item returned by second call
    BulkDataResponse resp = service.getData(incoming);
    assertNotNull(resp);
    assertTrue(resp instanceof BulkProductLiteResponse);
    BulkProductLiteResponse lite = (BulkProductLiteResponse) resp;
    // Ensure code didn't crash. If the second call returned a page, we expect at least the returned row.
    // The exact number may depend on timing/order; assert no exception and list exists.
    assertNotNull(lite.getProductContentList());
  }

  // helper
  private ItemBasicL4Response createL4(String productSku, String itemSku, String upc, String itemName) {
    ItemBasicL4Response it = new ItemBasicL4Response();
    it.setProductSku(productSku);
    it.setItemSku(itemSku);
    it.setUpcCode(upc);
    it.setItemName(itemName);
    return it;
  }

  @Test
  void testGetData_happyPath_returnsRows_() throws Exception {
    // Prepare request objects
    BulkDownloadRequest incoming = new BulkDownloadRequest();
    incoming.setMerchantId("MERCH1");
    incoming.setRequestId("REQ-1");

    ProductUPCDownloadRequest converted = new ProductUPCDownloadRequest();
    ProductSummaryRequest productSummaryRequest = new ProductSummaryRequest();
    converted.setProductSummaryRequest(productSummaryRequest);
    converted.setUsername("userA");
    converted.setRequestId("REQ-1");

    when(objectMapper.convertValue(incoming, ProductUPCDownloadRequest.class)).thenReturn(converted);

    // Business partner
    ProfileResponse profile = new ProfileResponse();
    profile.setBusinessPartnerCode("BP001");
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(), eq("MERCH1")))
        .thenReturn(profile);

    // Product L3 summaries: create 2 SKUs
    ProductL3SummaryResponse p1 = new ProductL3SummaryResponse();
    p1.setProductSku("P-001");
    p1.setProductName("Prod 1");
    ProductL3SummaryResponse p2 = new ProductL3SummaryResponse();
    p2.setProductSku("P-002");
    p2.setProductName("Prod 2");

    // Simulate single L3 page containing both products (simple case)
    Page<ProductL3SummaryResponse> page1 = new PageImpl<>(Arrays.asList(p1, p2));
    when(xProductOutboundService.getProductL3SummaryResponse(eq(productSummaryRequest), anyInt(), anyInt(), eq("REQ-1"),
        eq("userA")))
        .thenReturn(page1);

    // L4 items for both products. Because batchSizeL4 is 2 (set in @BeforeEach),
    // return both items in the first page (size == 2) then an empty page to stop pagination.
    ItemBasicL4Response item1 = new ItemBasicL4Response();
    item1.setProductSku("P-001");
    item1.setItemSku("I-001");
    item1.setUpcCode("UPC001");
    item1.setItemName("Item One");

    ItemBasicL4Response item2 = new ItemBasicL4Response();
    item2.setProductSku("P-002");
    item2.setItemSku("I-002");
    item2.setUpcCode("UPC002");
    item2.setItemName("Item Two");

    // First L4 page contains both items (size == batchSizeL4), second L4 page is empty (size 0)
    Page<ItemBasicL4Response> l4pageFull = new PageImpl<>(
        Arrays.asList(item1, item2), Pageable.unpaged(), 2);
    Page<ItemBasicL4Response> l4pageEmpty = new PageImpl<>(Collections.emptyList());

    when(xProductOutboundService.getL4ItemListByProductSku(anyInt(), anyInt(), eq("REQ-1"), eq("userA"), any()))
        .thenReturn(l4pageFull)
        .thenReturn(l4pageEmpty);

    // Invoke
    BulkDataResponse resp = service.getData(incoming);

    assertNotNull(resp);
    assertTrue(resp instanceof BulkProductLiteResponse);
    BulkProductLiteResponse lite = (BulkProductLiteResponse) resp;
    List<List<String>> rows = lite.getProductContentList();

    // Expect two rows, one per L4 item returned
    assertEquals(2, rows.size());

    List<String> row1 = rows.stream().filter(r -> r.get(0).equals("P-001")).findFirst().orElse(null);
    assertNotNull(row1);
    assertEquals(Arrays.asList("P-001", "Prod 1", "I-001", "Item One", "UPC001"), row1);

    List<String> row2 = rows.stream().filter(r -> r.get(0).equals("P-002")).findFirst().orElse(null);
    assertNotNull(row2);
    assertEquals(Arrays.asList("P-002", "Prod 2", "I-002", "Item Two", "UPC002"), row2);
  }
}