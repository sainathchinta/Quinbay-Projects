package com.gdn.x.product.dao.api;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.mongodb.repository.MongoRepository;

import com.gdn.x.product.model.entity.TicketTemplate;

public interface TicketTemplateRepository extends MongoRepository<TicketTemplate, String> {

  Page<TicketTemplate> findByStoreIdAndMarkForDeleteFalse(String storeId, Pageable pageable);

  public Page<TicketTemplate> findByStoreIdAndNameLikeIgnoreCaseAndMarkForDeleteFalse(
      String storeId, String regex, Pageable pageable);

  TicketTemplate findByTicketTemplateCode(String ticketTemplateCode);

  TicketTemplate findByTicketTemplateCodeAndMarkForDeleteFalse(String ticketTemplateCode);

}
