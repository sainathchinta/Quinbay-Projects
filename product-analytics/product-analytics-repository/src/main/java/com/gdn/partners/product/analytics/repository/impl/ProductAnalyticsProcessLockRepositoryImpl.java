package com.gdn.partners.product.analytics.repository.impl;

import java.util.Objects;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DuplicateKeyException;
import org.springframework.data.mongodb.core.FindAndModifyOptions;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.data.mongodb.core.query.Update;

import com.gdn.partners.product.analytics.entity.ProductAnalyticsProcess;
import com.gdn.partners.product.analytics.entity.ProductAnalyticsProcessLock;
import com.gdn.partners.product.analytics.model.FieldNames;
import com.gdn.partners.product.analytics.repository.ProductAnalyticsProcessLockRepositoryCustom;

import lombok.extern.slf4j.Slf4j;

@Slf4j
public class ProductAnalyticsProcessLockRepositoryImpl implements ProductAnalyticsProcessLockRepositoryCustom {

  @Autowired
  private MongoTemplate mongoTemplate;

  @Override
  public boolean insertLock(String lockName, long lockTimeInMillis, ProductAnalyticsProcess productAnalyticsProcess) {
    ProductAnalyticsProcessLock productAnalyticsProcessLock = ProductAnalyticsProcessLock.builder()
        .lockedAt(System.currentTimeMillis())
        .lockName(lockName)
        .lockUntil(System.currentTimeMillis() + lockTimeInMillis)
        .productAnalyticsProcess(productAnalyticsProcess)
        .build();
    try {
      mongoTemplate.insert(productAnalyticsProcessLock);
      return true;
    } catch (DuplicateKeyException e){
      return false;
    }
  }

  @Override
  public boolean acquireLockIfReleased(String lockName, long lockTimeInMillis,
      ProductAnalyticsProcess productAnalyticsProcess) {
    long lockUntilTime = System.currentTimeMillis() + lockTimeInMillis;
    Criteria criteria = new Criteria();
    criteria.andOperator(Criteria.where(FieldNames.LOCK_NAME).is(lockName),
        Criteria.where(FieldNames.LOCK_UNTIL).lte(System.currentTimeMillis()));
    Query query = Query.query(criteria);
    Update update = new Update();
    update.set(FieldNames.LOCK_UNTIL, lockUntilTime);
    update.set(FieldNames.PROCESS, productAnalyticsProcess);
    update.set(FieldNames.LOCKED_AT, System.currentTimeMillis());
    ProductAnalyticsProcessLock updatedProductAnalyticsProcess = mongoTemplate.findAndModify(query, update,
        new FindAndModifyOptions().returnNew(true), ProductAnalyticsProcessLock.class);
    return Objects.nonNull(updatedProductAnalyticsProcess);
  }

  @Override
  public boolean releaseLock(String lockName, ProductAnalyticsProcess productAnalyticsProcess) {
    Criteria criteria = new Criteria();
    criteria.andOperator(Criteria.where(FieldNames.LOCK_NAME).is(lockName),
        Criteria.where(FieldNames.PROCESS).is(productAnalyticsProcess));
    Query query = Query.query(criteria);
    Update update = Update.update(FieldNames.LOCK_UNTIL, System.currentTimeMillis());
    ProductAnalyticsProcessLock updatedProductAnalyticsProcess = mongoTemplate.findAndModify(query, update,
        new FindAndModifyOptions().returnNew(true), ProductAnalyticsProcessLock.class);
    return Objects.nonNull(updatedProductAnalyticsProcess);
  }
}
