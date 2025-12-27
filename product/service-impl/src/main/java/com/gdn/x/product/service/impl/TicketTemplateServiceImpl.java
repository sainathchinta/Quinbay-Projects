package com.gdn.x.product.service.impl;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.x.product.dao.api.TicketTemplateRepository;
import com.gdn.x.product.dao.solr.api.ProductAndItemSolrRepository;
import com.gdn.x.product.enums.SystemParameterNames;
import com.gdn.x.product.model.entity.TicketTemplate;
import com.gdn.x.product.model.solr.ProductAndItemSolr;
import com.gdn.x.product.service.api.ItemService;
import com.gdn.x.product.service.api.SystemParameterService;
import com.gdn.x.product.service.api.TicketTemplateService;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;

@Service
public class TicketTemplateServiceImpl implements TicketTemplateService {

  private static final String CHAR_UNDERSCORE = "_";

  private static final String CHAR_SPACE = " ";

  private static final String TICKET_TEMPLATE_CODE_IS_ALREADY_EXISTS =
      "Ticket template code is already exists";

  private static final String TICKET_TEMPLATE_NAME_MUST_NOT_BE_NULL =
      "ticket template name must not be null";

  private static final String TICKET_TEMPLATE_NOT_FOUND_WITH_CODE =
      "Ticket template not found with code ";

  @Autowired
  private TicketTemplateRepository ticketTemplateRepository;

  @Autowired
  private ItemService itemService;

  @Autowired
  private ProductAndItemSolrRepository productAndItemSolrRepository;

  @Autowired
  private SystemParameterService systemParameterService;

  @Override
  public void assignToItem(String storeId, List<String> itemSkus, String ticketTemplateCode) {
    this.itemService.assignTicketTemplateToItems(storeId, itemSkus, ticketTemplateCode);
  }

  private TicketTemplate constructWithDefaultValue(TicketTemplate ticketTemplate) {
    String storeId = ticketTemplate.getStoreId();
    if (StringUtils.isEmpty(ticketTemplate.getFrom())) {
      ticketTemplate.setFrom(this.systemParameterService.findValueByStoreIdAndVariable(storeId,
          SystemParameterNames.EMAIL_SENDER_DEFAULT).getValue());
    }

    if (StringUtils.isEmpty(ticketTemplate.getSubjectOrderReceived())) {
      ticketTemplate.setSubjectOrderReceived(this.systemParameterService
          .findValueByStoreIdAndVariable(storeId,
              SystemParameterNames.SUBJECT_ORDER_RECEIVED_DEFAULT).getValue());
    }

    if (StringUtils.isEmpty(ticketTemplate.getSubjectOrderPaid())) {
      ticketTemplate.setSubjectOrderPaid(this.systemParameterService.findValueByStoreIdAndVariable(
          storeId, SystemParameterNames.SUBJECT_ORDER_PAID_DEFAULT).getValue());
    }

    if (StringUtils.isEmpty(ticketTemplate.getSubjectBarcode())) {
      ticketTemplate.setSubjectBarcode(this.systemParameterService.findValueByStoreIdAndVariable(
          storeId, SystemParameterNames.BARCODE_SUBJECT_DEFAULT).getValue());
    }
    return ticketTemplate;
  }

  @Override
  public void delete(String storeId, String ticketTemplateCode) throws Exception {
    TicketTemplate ticketTemplate =
        this.findByTicketTemplateCodeAndMarkForDeleteFalse(ticketTemplateCode);
    GdnPreconditions.checkArgument(ticketTemplate != null,
        TicketTemplateServiceImpl.TICKET_TEMPLATE_NOT_FOUND_WITH_CODE + ticketTemplateCode);
    List<String> itemSkus = this.findItemSkuByTicketTemplateCode(ticketTemplateCode);
    if (!itemSkus.isEmpty()) {
      this.unassignToItem(storeId, itemSkus);
    }
    ticketTemplate.setMarkForDelete(true);
    this.ticketTemplateRepository.save(ticketTemplate);
  }

  @Override
  public Page<TicketTemplate> findByNameLike(String storeId, String name) {
    return this.ticketTemplateRepository.findByStoreIdAndNameLikeIgnoreCaseAndMarkForDeleteFalse(
        storeId, name, PageRequest.of(0, 10));
  }

  @Override
  public Page<TicketTemplate> findByStoreIdAndMarkForDeleteFalse(String storeId, Pageable pageable) {
    return this.ticketTemplateRepository.findByStoreIdAndMarkForDeleteFalse(storeId, pageable);
  }

  @Override
  public TicketTemplate findByTicketTemplateCodeAndMarkForDeleteFalse(String ticketTemplateCode) {
    return this.ticketTemplateRepository
        .findByTicketTemplateCodeAndMarkForDeleteFalse(ticketTemplateCode);
  }

  @Override
  public List<String> findItemSkuByTicketTemplateCode(String ticketTemplateCode) {
    List<String> itemSkus = new ArrayList<String>();
    for (ProductAndItemSolr itemSolr : this
        .findProductAndItemSolrByTicketTemplateCode(ticketTemplateCode)) {
      itemSkus.add(itemSolr.getItemSku());
    }
    return itemSkus;
  }

  @Override
  public List<ProductAndItemSolr> findProductAndItemSolrByTicketTemplateCode(
      String ticketTemplateCode) {
    return this.productAndItemSolrRepository
        .findByTicketTemplateCodeAndMarkForDeleteFalse(ticketTemplateCode);
  }

  @Override
  public String save(String storeId, TicketTemplate ticketTemplate) throws Exception {
    GdnPreconditions.checkArgument(ticketTemplate.getName() != null,
        TicketTemplateServiceImpl.TICKET_TEMPLATE_NAME_MUST_NOT_BE_NULL);
    String ticketTemplateCode =
        ticketTemplate
            .getName()
            .replace(TicketTemplateServiceImpl.CHAR_SPACE,
                TicketTemplateServiceImpl.CHAR_UNDERSCORE).toUpperCase();
    TicketTemplate savedTicketTemplate =
        this.ticketTemplateRepository.findByTicketTemplateCode(ticketTemplateCode);
    GdnPreconditions.checkArgument(savedTicketTemplate == null,
        TicketTemplateServiceImpl.TICKET_TEMPLATE_CODE_IS_ALREADY_EXISTS);
    ticketTemplate.setStoreId(storeId);
    ticketTemplate.setTicketTemplateCode(ticketTemplateCode);
    TicketTemplate result =
        this.ticketTemplateRepository.save(this.constructWithDefaultValue(ticketTemplate));
    return result.getTicketTemplateCode();
  }

  @Override
  public void unassignToItem(String storeId, List<String> itemSkus) {
    this.itemService.assignTicketTemplateToItems(storeId, itemSkus, null);
  }

  @Override
  public void update(TicketTemplate ticketTemplate) throws Exception {
    TicketTemplate savedTicketTemplate =
        this.findByTicketTemplateCodeAndMarkForDeleteFalse(ticketTemplate.getTicketTemplateCode());
    BeanUtils.copyProperties(ticketTemplate, savedTicketTemplate, "createdBy", "createdDate",
        "version", "id", "storeId");
    this.ticketTemplateRepository.save(savedTicketTemplate);
  }
}
