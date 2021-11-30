import { HttpClient } from '@angular/common/http';
import { Component, OnInit } from '@angular/core';
import { Subject, of, timer } from 'rxjs';
import { takeUntil, switchMap, catchError, tap } from 'rxjs/operators';
import { environment } from 'src/environments/environment';

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

    private readonly die$: Subject<void> = new Subject<void>();

    private openedServices: string[] = [];

    constructor(private readonly http: HttpClient) {
    }

    ngOnInit(): void {
        timer(0, 1000)
            .pipe(
                takeUntil(this.die$),
                switchMap(_ => this.http.get<IService[]>(`http://${environment.backend}:${environment.port}/api/health`)),
                catchError((err, _) => {
                    console.error(err);
                    return of([]);
                }),
                tap(services => this.services = services))
            .subscribe();
    }

    ngOnDestroy(): void {
        this.die$.complete();
    }

    keysof(obj: object): [string, any][] {
        return Object.entries(obj);
    }

    isOpened(id: string): boolean {
        return this.openedServices.includes(id);
    }

    opened(id: string) {
        if (!this.isOpened(id))
            this.openedServices.push(id);
    }

    closed(id: string) {
        this.openedServices = this.openedServices.filter(otherID => otherID !== id);
    }

}
