package com.gdn.x.product.service.api;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import com.gdn.x.product.model.entity.TicketTemplate;
import com.gdn.x.product.model.solr.ProductAndItemSolr;

public interface TicketTemplateService {

  void assignToItem(String storeId, List<String> itemSkus, String ticketTemplateCode);

  void delete(String storeId, String ticketTemplateCode) throws Exception;

  Page<TicketTemplate> findByNameLike(String storeId, String name);

  Page<TicketTemplate> findByStoreIdAndMarkForDeleteFalse(String storeId, Pageable pageable)
      throws Exception;

  TicketTemplate findByTicketTemplateCodeAndMarkForDeleteFalse(String ticketTemplateCode);

  List<String> findItemSkuByTicketTemplateCode(String ticketTemplateCode);

  List<ProductAndItemSolr> findProductAndItemSolrByTicketTemplateCode(String ticketTemplateCode);

  String save(String storeId, TicketTemplate ticketTemplate) throws Exception;

  void unassignToItem(String storeId, List<String> itemSkus);

  void update(TicketTemplate ticketTemplate) throws Exception;

}
