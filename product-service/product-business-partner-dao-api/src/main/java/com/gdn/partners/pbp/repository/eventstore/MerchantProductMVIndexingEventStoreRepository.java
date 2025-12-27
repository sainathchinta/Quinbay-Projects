package com.gdn.partners.pbp.repository.eventstore;

import org.springframework.data.jpa.repository.JpaRepository;

import com.gdn.partners.pbp.entity.eventstore.MerchantProductMVIndexingEventStore;

public interface MerchantProductMVIndexingEventStoreRepository extends
    JpaRepository<MerchantProductMVIndexingEventStore, String> {


}
