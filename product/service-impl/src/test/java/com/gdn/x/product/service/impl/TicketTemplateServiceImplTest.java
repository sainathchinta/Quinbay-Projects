package com.gdn.x.product.service.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.openMocks;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.product.dao.api.TicketTemplateRepository;
import com.gdn.x.product.dao.solr.api.ProductAndItemSolrRepository;
import com.gdn.x.product.enums.SystemParameterNames;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.SystemParameter;
import com.gdn.x.product.model.entity.TicketTemplate;
import com.gdn.x.product.model.solr.ProductAndItemSolr;
import com.gdn.x.product.service.api.ItemService;
import com.gdn.x.product.service.api.SystemParameterService;

public class TicketTemplateServiceImplTest {

  private static final String SUBJECT_ORDER_RECEIVED = "subjectOrderReceived";
  private static final String FROM = "FROM";
  private static final String SUBJECT_ORDER_PAID = "SUBJECT_ORDER_PAID";
  private static final String SUBJECT_BARCODE = "SUBJECT_BARCODE";
  private static final String DESCRIPTION = "description";
  private static final PageRequest PAGEABLE = PageRequest.of(0, 1);
  private static final String PRODUCT_SKU = "productSku";
  private static final String ITEM_SKU = "itemSku";
  private static final String STORE_ID = "storeId";
  private static final String TICKET_TEMPLATE_CODE = "TICKET_TEMPLATE_NAME";
  private static final String TICKET_TEMPLATE_NAME = "ticket template name";

  @InjectMocks
  private TicketTemplateServiceImpl ticketTemplateServiceImpl;

  @Mock
  private ItemService itemService;

  @Mock
  private TicketTemplateRepository ticketTemplateRepository;

  @Mock
  private ProductAndItemSolrRepository productAndItemSolrRepository;

  @Mock
  private SystemParameterService systemParameterService;

  private List<String> itemSkus;

  private List<Item> items;
  private Item item;
  private TicketTemplate ticketTemplate;
  private ProductAndItemSolr productAndItemSolr;
  private List<ProductAndItemSolr> productAndItemSolrs;
  private ArrayList<TicketTemplate> ticketTemplates;
  private PageImpl<TicketTemplate> pageImpl;

  @Test
  public void assignToItemTest() {
    when(
        this.itemService.assignTicketTemplateToItems(TicketTemplateServiceImplTest.STORE_ID,
            this.itemSkus, TicketTemplateServiceImplTest.TICKET_TEMPLATE_CODE)).thenReturn(
        this.items);
    this.ticketTemplateServiceImpl.assignToItem(TicketTemplateServiceImplTest.STORE_ID,
        this.itemSkus, TicketTemplateServiceImplTest.TICKET_TEMPLATE_CODE);
    verify(this.itemService).assignTicketTemplateToItems(TicketTemplateServiceImplTest.STORE_ID,
        this.itemSkus, TicketTemplateServiceImplTest.TICKET_TEMPLATE_CODE);
  }

  @Test
  public void deleteNotFoundTest() {
    try {
      when(
          this.ticketTemplateRepository
              .findByTicketTemplateCodeAndMarkForDeleteFalse(TicketTemplateServiceImplTest.TICKET_TEMPLATE_CODE))
          .thenReturn(null);
      this.ticketTemplateServiceImpl.delete(TicketTemplateServiceImplTest.STORE_ID,
          TicketTemplateServiceImplTest.TICKET_TEMPLATE_CODE);
    } catch (Exception e) {
      assertTrue(e instanceof ApplicationRuntimeException);
      verify(this.ticketTemplateRepository).findByTicketTemplateCodeAndMarkForDeleteFalse(
          TicketTemplateServiceImplTest.TICKET_TEMPLATE_CODE);
    }
  }


  @Test
  public void deleteTest() throws Exception {
    when(
        this.ticketTemplateRepository
            .findByTicketTemplateCodeAndMarkForDeleteFalse(TicketTemplateServiceImplTest.TICKET_TEMPLATE_CODE))
        .thenReturn(this.ticketTemplate);
    when(
        this.productAndItemSolrRepository
            .findByTicketTemplateCodeAndMarkForDeleteFalse(TicketTemplateServiceImplTest.TICKET_TEMPLATE_CODE))
        .thenReturn(this.productAndItemSolrs);
    this.ticketTemplateServiceImpl.delete(TicketTemplateServiceImplTest.STORE_ID,
        TicketTemplateServiceImplTest.TICKET_TEMPLATE_CODE);
    verify(this.ticketTemplateRepository).findByTicketTemplateCodeAndMarkForDeleteFalse(
        TicketTemplateServiceImplTest.TICKET_TEMPLATE_CODE);
    verify(this.productAndItemSolrRepository).findByTicketTemplateCodeAndMarkForDeleteFalse(
        TicketTemplateServiceImplTest.TICKET_TEMPLATE_CODE);
    verify(this.itemService).assignTicketTemplateToItems(TicketTemplateServiceImplTest.STORE_ID,
        this.itemSkus, null);
    verify(this.ticketTemplateRepository).save(this.ticketTemplate);
  }


  @Test
  public void deleteTestWithItemSkusEmpty() throws Exception {
    when(
        this.ticketTemplateRepository
            .findByTicketTemplateCodeAndMarkForDeleteFalse(TicketTemplateServiceImplTest.TICKET_TEMPLATE_CODE))
        .thenReturn(this.ticketTemplate);
    this.productAndItemSolrs = new ArrayList<ProductAndItemSolr>();
    when(
        this.productAndItemSolrRepository
            .findByTicketTemplateCodeAndMarkForDeleteFalse(TicketTemplateServiceImplTest.TICKET_TEMPLATE_CODE))
        .thenReturn(this.productAndItemSolrs);
    this.ticketTemplateServiceImpl.delete(TicketTemplateServiceImplTest.STORE_ID,
        TicketTemplateServiceImplTest.TICKET_TEMPLATE_CODE);
    verify(this.ticketTemplateRepository).findByTicketTemplateCodeAndMarkForDeleteFalse(
        TicketTemplateServiceImplTest.TICKET_TEMPLATE_CODE);
    verify(this.productAndItemSolrRepository).findByTicketTemplateCodeAndMarkForDeleteFalse(
        TicketTemplateServiceImplTest.TICKET_TEMPLATE_CODE);
    verify(this.ticketTemplateRepository).save(this.ticketTemplate);
  }


  @Test
  public void findByNameLikeTest() {
    this.ticketTemplateServiceImpl.findByNameLike(TicketTemplateServiceImplTest.STORE_ID,
        TicketTemplateServiceImplTest.TICKET_TEMPLATE_NAME);
    verify(this.ticketTemplateRepository).findByStoreIdAndNameLikeIgnoreCaseAndMarkForDeleteFalse(
        TicketTemplateServiceImplTest.STORE_ID, TicketTemplateServiceImplTest.TICKET_TEMPLATE_NAME,
        PageRequest.of(0, 10));
  }

  @Test
  public void findByStoreIdAndMarkForDeleteFalseTest() {
    when(
        this.ticketTemplateRepository.findByStoreIdAndMarkForDeleteFalse(
            TicketTemplateServiceImplTest.STORE_ID, TicketTemplateServiceImplTest.PAGEABLE))
        .thenReturn(this.pageImpl);
    Page<TicketTemplate> result =
        this.ticketTemplateServiceImpl.findByStoreIdAndMarkForDeleteFalse(
            TicketTemplateServiceImplTest.STORE_ID, TicketTemplateServiceImplTest.PAGEABLE);
    assertEquals(result, this.pageImpl);
    verify(this.ticketTemplateRepository).findByStoreIdAndMarkForDeleteFalse(
        TicketTemplateServiceImplTest.STORE_ID, TicketTemplateServiceImplTest.PAGEABLE);
  }

  @Test
  public void findByTicketTemplateCodeAndMarkForDeleteFalseTest() {
    when(
        this.ticketTemplateRepository
            .findByTicketTemplateCodeAndMarkForDeleteFalse(TicketTemplateServiceImplTest.TICKET_TEMPLATE_CODE))
        .thenReturn(this.ticketTemplate);
    TicketTemplate result =
        this.ticketTemplateServiceImpl
            .findByTicketTemplateCodeAndMarkForDeleteFalse(TicketTemplateServiceImplTest.TICKET_TEMPLATE_CODE);
    assertEquals(result, this.ticketTemplate);
    verify(this.ticketTemplateRepository).findByTicketTemplateCodeAndMarkForDeleteFalse(
        TicketTemplateServiceImplTest.TICKET_TEMPLATE_CODE);
  }

  @Test
  public void saveTest() throws Exception {
    when(this.ticketTemplateRepository.save(this.ticketTemplate)).thenReturn(this.ticketTemplate);
    this.ticketTemplate.setSubjectBarcode(TicketTemplateServiceImplTest.SUBJECT_BARCODE);
    this.ticketTemplate.setSubjectOrderPaid(TicketTemplateServiceImplTest.SUBJECT_ORDER_PAID);
    this.ticketTemplate
        .setSubjectOrderReceived(TicketTemplateServiceImplTest.SUBJECT_ORDER_RECEIVED);
    this.ticketTemplate.setFrom(TicketTemplateServiceImplTest.FROM);
    String result =
        this.ticketTemplateServiceImpl.save(TicketTemplateServiceImplTest.STORE_ID,
            this.ticketTemplate);
    assertEquals(result, TicketTemplateServiceImplTest.TICKET_TEMPLATE_CODE);
    verify(this.ticketTemplateRepository).findByTicketTemplateCode(
        TicketTemplateServiceImplTest.TICKET_TEMPLATE_CODE);
    verify(this.ticketTemplateRepository).save(this.ticketTemplate);
  }

  @Test
  public void saveTestUsingDefaultValues() throws Exception {
    when(this.ticketTemplateRepository.save(this.ticketTemplate)).thenReturn(this.ticketTemplate);
    when(
        this.systemParameterService.findValueByStoreIdAndVariable(
            TicketTemplateServiceImplTest.STORE_ID, SystemParameterNames.BARCODE_SUBJECT_DEFAULT))
        .thenReturn(
            new SystemParameter(TicketTemplateServiceImplTest.STORE_ID,
                SystemParameterNames.BARCODE_SUBJECT_DEFAULT,
                SystemParameterNames.BARCODE_SUBJECT_DEFAULT,
                TicketTemplateServiceImplTest.DESCRIPTION));
    when(
        this.systemParameterService.findValueByStoreIdAndVariable(
            TicketTemplateServiceImplTest.STORE_ID, SystemParameterNames.EMAIL_SENDER_DEFAULT))
        .thenReturn(
            new SystemParameter(TicketTemplateServiceImplTest.STORE_ID,
                SystemParameterNames.EMAIL_SENDER_DEFAULT,
                SystemParameterNames.EMAIL_SENDER_DEFAULT,
                TicketTemplateServiceImplTest.DESCRIPTION));
    when(
        this.systemParameterService
            .findValueByStoreIdAndVariable(TicketTemplateServiceImplTest.STORE_ID,
                SystemParameterNames.SUBJECT_ORDER_PAID_DEFAULT)).thenReturn(
        new SystemParameter(TicketTemplateServiceImplTest.STORE_ID,
            SystemParameterNames.SUBJECT_ORDER_PAID_DEFAULT,
            SystemParameterNames.SUBJECT_ORDER_PAID_DEFAULT,
            TicketTemplateServiceImplTest.DESCRIPTION));
    when(
        this.systemParameterService.findValueByStoreIdAndVariable(
            TicketTemplateServiceImplTest.STORE_ID,
            SystemParameterNames.SUBJECT_ORDER_RECEIVED_DEFAULT)).thenReturn(
        new SystemParameter(TicketTemplateServiceImplTest.STORE_ID,
            SystemParameterNames.SUBJECT_ORDER_RECEIVED_DEFAULT,
            SystemParameterNames.SUBJECT_ORDER_RECEIVED_DEFAULT,
            TicketTemplateServiceImplTest.DESCRIPTION));
    String result =
        this.ticketTemplateServiceImpl.save(TicketTemplateServiceImplTest.STORE_ID,
            this.ticketTemplate);
    assertEquals(result, TicketTemplateServiceImplTest.TICKET_TEMPLATE_CODE);
    verify(this.ticketTemplateRepository).findByTicketTemplateCode(
        TicketTemplateServiceImplTest.TICKET_TEMPLATE_CODE);
    verify(this.ticketTemplateRepository).save(this.ticketTemplate);
    verify(this.systemParameterService).findValueByStoreIdAndVariable(
        TicketTemplateServiceImplTest.STORE_ID, SystemParameterNames.BARCODE_SUBJECT_DEFAULT);
    verify(this.systemParameterService).findValueByStoreIdAndVariable(
        TicketTemplateServiceImplTest.STORE_ID, SystemParameterNames.EMAIL_SENDER_DEFAULT);
    verify(this.systemParameterService).findValueByStoreIdAndVariable(
        TicketTemplateServiceImplTest.STORE_ID, SystemParameterNames.SUBJECT_ORDER_PAID_DEFAULT);
    verify(this.systemParameterService)
        .findValueByStoreIdAndVariable(TicketTemplateServiceImplTest.STORE_ID,
            SystemParameterNames.SUBJECT_ORDER_RECEIVED_DEFAULT);
  }

  @BeforeEach
  public void setUp() {
    openMocks(this);
    this.itemSkus = new ArrayList<String>();
    this.itemSkus.add(TicketTemplateServiceImplTest.ITEM_SKU);
    this.item =
        new Item(TicketTemplateServiceImplTest.ITEM_SKU, TicketTemplateServiceImplTest.PRODUCT_SKU);
    this.items = new ArrayList<Item>();
    this.items.add(this.item);

    this.ticketTemplate = new TicketTemplate();
    this.ticketTemplate.setTicketTemplateCode(TicketTemplateServiceImplTest.TICKET_TEMPLATE_CODE);
    this.ticketTemplate.setName(TicketTemplateServiceImplTest.TICKET_TEMPLATE_CODE);
    this.ticketTemplates = new ArrayList<TicketTemplate>();
    this.ticketTemplates.add(this.ticketTemplate);

    this.productAndItemSolr = new ProductAndItemSolr();
    this.productAndItemSolr.setItemSku(TicketTemplateServiceImplTest.ITEM_SKU);
    this.productAndItemSolrs = new ArrayList<ProductAndItemSolr>();
    this.productAndItemSolrs.add(this.productAndItemSolr);

    this.pageImpl = new PageImpl<TicketTemplate>(this.ticketTemplates);
    this.itemSkus = new ArrayList<String>();
    this.itemSkus.add(TicketTemplateServiceImplTest.ITEM_SKU);
    this.item =
        new Item(TicketTemplateServiceImplTest.ITEM_SKU, TicketTemplateServiceImplTest.PRODUCT_SKU);
    this.items = new ArrayList<Item>();
    this.items.add(this.item);

    this.ticketTemplate = new TicketTemplate();
    this.ticketTemplate.setName(TicketTemplateServiceImplTest.TICKET_TEMPLATE_NAME);
    this.ticketTemplates = new ArrayList<TicketTemplate>();
    this.ticketTemplates.add(this.ticketTemplate);

    this.productAndItemSolr = new ProductAndItemSolr();
    this.productAndItemSolr.setItemSku(TicketTemplateServiceImplTest.ITEM_SKU);
    this.productAndItemSolrs = new ArrayList<ProductAndItemSolr>();
    this.productAndItemSolrs.add(this.productAndItemSolr);

    this.pageImpl = new PageImpl<TicketTemplate>(this.ticketTemplates);
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(this.itemService);
    verifyNoMoreInteractions(this.ticketTemplateRepository);
    verifyNoMoreInteractions(this.productAndItemSolrRepository);
  }

  @Test
  public void unassignToItemTest() {

  }

  @Test
  public void updateTest() throws Exception {
    this.ticketTemplate.setTicketTemplateCode(TicketTemplateServiceImplTest.TICKET_TEMPLATE_CODE);
    when(
        this.ticketTemplateRepository
            .findByTicketTemplateCodeAndMarkForDeleteFalse(TicketTemplateServiceImplTest.TICKET_TEMPLATE_CODE))
        .thenReturn(this.ticketTemplate);
    this.ticketTemplateServiceImpl.update(this.ticketTemplate);
    verify(this.ticketTemplateRepository).findByTicketTemplateCodeAndMarkForDeleteFalse(
        TicketTemplateServiceImplTest.TICKET_TEMPLATE_CODE);
    verify(this.ticketTemplateRepository).save(this.ticketTemplate);
  }

}
