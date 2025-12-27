package com.gdn.partners.pbp.repository.eventstore;

import org.springframework.data.jpa.repository.JpaRepository;

import com.gdn.partners.pbp.entity.eventstore.EventStore;

public interface EventStoreRepository extends JpaRepository<EventStore, String>,
    EventStoreRepositoryCustom {
}
