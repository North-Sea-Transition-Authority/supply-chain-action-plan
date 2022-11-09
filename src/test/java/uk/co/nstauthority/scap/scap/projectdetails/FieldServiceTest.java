package uk.co.nstauthority.scap.scap.projectdetails;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.entry;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.fivium.energyportalapi.client.field.FieldApi;
import uk.co.fivium.energyportalapi.generated.client.FieldProjectionRoot;
import uk.co.fivium.energyportalapi.generated.client.FieldsProjectionRoot;
import uk.co.fivium.energyportalapi.generated.types.Field;
import uk.co.fivium.energyportalapi.generated.types.FieldStatus;
import uk.co.nstauthority.scap.energyportal.FieldService;

@ExtendWith(MockitoExtension.class)
class FieldServiceTest {

  @Mock
  FieldApi fieldApi;

  @InjectMocks
  FieldService fieldService;

  List<Field> fields;

  @BeforeEach
  void setup() {
    fields = List.of(
        new Field(1, "test field 1", null, null, null, null),
        new Field(2, "test field 2", null, null, null, null)
    );
  }

  @Test
  void getFieldsByName_assertReturn() {
    var searchTerm = "test";
    var purpose = "Test mock api request purpose";
    var statuses = Arrays.stream(FieldStatus.values())
        .filter(fieldStatus -> fieldStatus != FieldStatus.STATUS9999)
        .toList();
    var argumentCaptor = ArgumentCaptor.forClass(FieldsProjectionRoot.class);

    when(fieldApi.searchFields(eq(searchTerm), eq(statuses), any(), eq(purpose)))
        .thenReturn(fields);

    var queryResults = fieldService.getFieldsByName(searchTerm, purpose);

    verify(fieldApi)
        .searchFields(eq(searchTerm), eq(statuses), argumentCaptor.capture(), eq(purpose));

    var requestedParams = argumentCaptor.getValue().getFields();

    assertThat(queryResults).isEqualTo(fields);
    assertThat(requestedParams).containsExactly(
        entry("fieldId", null),
        entry("fieldName", null)
    );
  }

  @Test
  void getFieldById() {
    var requestedFieldId = 34;
    var requestPurpose = "Test request purpose";
    var field = new Field(requestedFieldId, "Test field name", null, null, null, null);
    var argumentCaptor = ArgumentCaptor.forClass(FieldProjectionRoot.class);

    when(fieldApi.findFieldById(eq(requestedFieldId), any(FieldProjectionRoot.class), eq(requestPurpose)))
        .thenReturn(Optional.of(field));

    var returnedField = fieldService.getFieldById(requestedFieldId, requestPurpose);

    verify(fieldApi)
        .findFieldById(eq(requestedFieldId), argumentCaptor.capture(), eq(requestPurpose));
    var requestedParams = argumentCaptor.getValue().getFields();
    assertThat(requestedParams).containsExactly(
        entry("fieldId", null),
        entry("fieldName", null)
    );
    assertThat(returnedField).contains(field);
  }

  @Test
  void doesFieldExist_doesExist_assertTrue() {
    var existingFieldId = 1;
    var purpose = "Validate that Field exists for SCAP project details";
    when(fieldApi.findFieldById(eq(existingFieldId), any(FieldProjectionRoot.class), eq(purpose)))
        .thenReturn(Optional.of(fields.get(0)));

    assertTrue(fieldService.doesFieldExist(1));
  }

  @Test
  void doesFieldExist_doesNotExist_assertFalse() {
    var existingFieldId = 3;
    var purpose = "Validate that Field exists for SCAP project details";
    when(fieldApi.findFieldById(eq(existingFieldId), any(FieldProjectionRoot.class), eq(purpose)))
        .thenReturn(Optional.empty());

    assertFalse(fieldService.doesFieldExist(3));
  }
}
