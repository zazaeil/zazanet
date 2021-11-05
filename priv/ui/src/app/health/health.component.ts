import { HttpClient } from '@angular/common/http';
import { Component, OnInit } from '@angular/core';
import { of } from 'rxjs';
import { catchError, tap } from 'rxjs/operators';

interface IService {
  service: string
  health: 'green' | 'yellow' | 'red'
  info: Map<string, any>
}

@Component({
  selector: 'app-health',
  templateUrl: './health.component.html',
  styleUrls: ['./health.component.css']
})
export class HealthComponent implements OnInit {

  public services: IService[] = [];

  constructor(private http: HttpClient) { }

  ngOnInit(): void {
    this.http.get<IService[]>('/api/health')
      .pipe(
        catchError((err, _) => {
          console.error(err);
          return of([]);
        }),
        tap(services => this.services = services))
      .subscribe();
  }

  keysof(obj: object): [string, any][] {
    return Object.entries(obj);
  }

}
