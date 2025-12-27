package com.gdn.mta.domain.event.modal;

import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import com.gdn.mta.product.valueobject.SolrProductCollectionDTO;
import java.io.Serializable;

public class SolrProductCollectionUpdateEvent extends GdnBaseDomainEventModel implements Serializable {

    private static final long serialVersionUID = 7570639239815035747L;
    private SolrProductCollectionDTO solrProductCollectionDTO;
    private String documentId;

    public SolrProductCollectionUpdateEvent() {
    }

    public String getDocumentId() {
        return documentId;
    }

    public void setDocumentId(String documentId) {
        this.documentId = documentId;
    }

    public SolrProductCollectionUpdateEvent(SolrProductCollectionDTO solrProductCollectionDTO) {
        this.solrProductCollectionDTO = solrProductCollectionDTO;
    }

    public SolrProductCollectionDTO getSolrProductCollectionDTO() {
        return solrProductCollectionDTO;
    }

    public void setSolrProductCollectionDTO(SolrProductCollectionDTO solrProductCollectionDTO) {
        this.solrProductCollectionDTO = solrProductCollectionDTO;
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("SolrProductCollectionUpdateEvent{");
        sb.append("productCollectionResponse=").append(solrProductCollectionDTO);
        sb.append('}');
        return sb.toString();
    }
}
