package uk.co.nstauthority.scap.restapi.scap;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import uk.co.nstauthority.scap.fds.searchselector.RestSearchResult;

@RestController
@RequestMapping("/data/scap")
public class ScapRestController {

  private final ScapRestService scapRestService;

  @Autowired
  ScapRestController(ScapRestService scapRestService) {
    this.scapRestService = scapRestService;
  }

  @GetMapping
  public RestSearchResult searchScaps(@RequestParam(value = "term", required = false) String searchTerm) {
    return scapRestService.searchScaps(searchTerm);
  }
}
