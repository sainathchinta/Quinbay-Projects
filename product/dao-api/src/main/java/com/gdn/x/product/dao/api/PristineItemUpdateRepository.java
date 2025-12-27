package com.gdn.x.product.dao.api;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.mongodb.repository.MongoRepository;

import com.gdn.x.product.model.entity.PristineDataItemUpdate;

public interface PristineItemUpdateRepository
    extends MongoRepository<PristineDataItemUpdate, String>, PristineItemUpdateRepositoryCustom {

  Page<PristineDataItemUpdate> findByIsUpdatedFalse(Pageable pageable);

  PristineDataItemUpdate findByPristineId(String pristineId);
}
